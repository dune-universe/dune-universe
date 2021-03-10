/* This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * Chunks of this code have been borrowed and influenced
 * by flac/decode.c and the flac XMMS plugin.
 *
 */

#include <memory.h>
#include <ogg/ogg.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <ocaml-ogg.h>

#include "flac_stubs.h"

typedef struct ocaml_flac_ogg_private {
  /* This is used by the decoder. */
  unsigned char *data;
  long bytes;
  long offset;
  /* This is used by the encoder. */
  ogg_int64_t granulepos;
  ogg_int64_t packetno;
  int header_count;
  value init_c;
  /* This is used by both. */
  value os;
} ocaml_flac_ogg_private;

static void finalize_private(ocaml_flac_ogg_private *p) {
  if (p->data != NULL)
    free(p->data);
  caml_remove_generational_global_root(&p->os);
  caml_remove_generational_global_root(&p->init_c);
  free(p);
}

static void finalize_ogg_decoder(value e) {
  ocaml_flac_decoder *dec = Decoder_val(e);
  finalize_private(dec->callbacks.private);
  finalize_decoder(e);
}

static struct custom_operations ogg_decoder_ops = {
    "ocaml_flac_ogg_decoder", finalize_ogg_decoder,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

/* C.f. http://flac.sourceforge.net/ogg_mapping.html */
CAMLprim value ocaml_flac_decoder_check_ogg(value v) {
  CAMLparam1(v);
  ogg_packet *p = Packet_val(v);
  unsigned char *h = p->packet;
  if (p->bytes < 9 ||
      /* FLAC */
      h[0] != 0x7f || h[1] != 'F' || h[2] != 'L' || h[3] != 'A' || h[4] != 'C')
    CAMLreturn(Val_false);

  CAMLreturn(Val_true);
}

/* libFLAC is monothread so this
 * is run within the main C thread.
 *
 * Ogg/flac mapping says:
 * "each packet corresponds to one FLAC audio frame."
 * and we decode frame by frame, so we only need to push one packet at a
 * time here. */
static FLAC__StreamDecoderReadStatus
ogg_read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[],
                  size_t *bytes, void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  ocaml_flac_ogg_private *h = (ocaml_flac_ogg_private *)callbacks->private;

  caml_acquire_runtime_system();

  int is_fresh;
  long offset;
  long data_bytes;
  unsigned char *data;

  if (h->data == NULL) {
    /* Grap a new ogg_packet */
    ogg_packet op;

    ogg_stream_state *os = Stream_state_val(h->os);
    int ret = ogg_stream_packetout(os, &op);
    if (ret == 0)
      caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    data = op.packet;
    data_bytes = op.bytes;
    offset = 0;
    is_fresh = 1;
  } else {
    data = h->data;
    data_bytes = h->bytes;
    offset = h->offset;
    is_fresh = 0;
  }

  long len;
  /* len is either *bytes or data_bytes-offset. */
  if (data_bytes - offset > *bytes)
    len = *bytes;
  else
    len = data_bytes - offset;

  memcpy(buffer, data + offset, len);

  /* Here we wrote all the data we  had, which was less than the required
   * amount. */
  if (len == data_bytes - offset) {
    if (is_fresh == 0) {
      free(h->data);
      h->data = NULL;
      h->bytes = 0;
      h->offset = 0;
    }
    /* Here, we have some data left so we save it for later.. */
  } else {
    if (is_fresh == 1) {
      long rem = data_bytes - offset - len;
      h->data = malloc(rem);
      if (h->data == NULL)
        caml_raise_out_of_memory();

      memcpy(h->data, data + offset + len, rem);
      h->bytes = rem;
      h->offset = 0;
    } else
      h->offset = offset + len;
  }

  caml_release_runtime_system();

  *bytes = len;
  if (len == 0)
    return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
  else
    return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

CAMLprim value ocaml_flac_decoder_ogg_update_os(value v, value os) {
  CAMLparam2(v, os);
  ocaml_flac_decoder *dec = Decoder_val(v);
  ocaml_flac_ogg_private *priv = dec->callbacks.private;
  caml_modify_generational_global_root(&priv->os, os);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_flac_decoder_ogg_create(value v, value os) {
  CAMLparam2(v, os);
  CAMLlocal1(ans);
  ogg_packet *p = Packet_val(v);

  ans = ocaml_flac_decoder_alloc(&ogg_decoder_ops);
  ocaml_flac_decoder *dec = Decoder_val(ans);

  ocaml_flac_ogg_private *priv = malloc(sizeof(ocaml_flac_ogg_private));
  if (priv == NULL)
    caml_raise_out_of_memory();

  priv->data = malloc(p->bytes);
  if (priv->data == NULL)
    caml_raise_out_of_memory();
  memcpy(priv->data, p->packet, p->bytes);
  priv->bytes = p->bytes;
  priv->offset = 9;
  priv->os = os;
  caml_register_generational_global_root(&priv->os);
  priv->init_c = Val_none;
  caml_register_generational_global_root(&priv->init_c);

  dec->callbacks.private = (void *)priv;

  // Intialize decoder
  caml_release_runtime_system();
  FLAC__stream_decoder_init_stream(dec->decoder, ogg_read_callback, NULL, NULL,
                                   NULL, NULL, dec_write_callback,
                                   dec_metadata_callback, dec_error_callback,
                                   (void *)&dec->callbacks);
  caml_acquire_runtime_system();

  CAMLreturn(ans);
}

/* Encoder */

static void finalize_ogg_encoder(value e) {
  ocaml_flac_encoder *dec = Encoder_val(e);
  finalize_private(dec->callbacks.private);
  finalize_encoder(e);
}

static struct custom_operations ogg_encoder_ops = {
    "ocaml_flac_ogg_encoder", finalize_ogg_encoder,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

FLAC__StreamEncoderWriteStatus ogg_enc_write_callback(
    const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes,
    unsigned samples, unsigned current_frame, void *client_data)

{
  /* See: http://flac.sourceforge.net/ogg_mapping.html
   * We have two header packets: stream info and metadata. */
  unsigned char header[51] = {
      0x7f, 'F', 'L', 'A', 'C', 0x01, 0x00, 0x00, 0x02, 'f', 'L', 'a', 'C',
      0,    0,   0,   0,   0,   0,    0,    0,    0,    0,   0,   0,   0,
      0,    0,   0,   0,   0,   0,    0,    0,    0,    0,   0,   0,   0,
      0,    0,   0,   0,   0,   0,    0,    0,    0,    0,   0,   0};

  ocaml_flac_encoder_callbacks *callbacks =
      (ocaml_flac_encoder_callbacks *)client_data;
  ocaml_flac_ogg_private *h = (ocaml_flac_ogg_private *)callbacks->private;

  /* Take the lock to prevent the Gc from accessing
   * concurrently this variable. */
  caml_acquire_runtime_system();
  ogg_stream_state *os = Stream_state_val(h->os);
  caml_release_runtime_system();

  /* Grab a new ogg_packet */
  ogg_packet op;
  /* Packet with samples are
   * normal packets. */
  if (samples > 0) {
    op.packet = (unsigned char *)buffer;
    op.bytes = bytes;
    h->granulepos += samples;
    h->packetno++;
    op.granulepos = h->granulepos;
    op.packetno = h->packetno;
    op.b_o_s = 0;
    /* We close the stream manually later. */
    op.e_o_s = 0;
    ogg_stream_packetin(os, &op);
    /* Packet with no samples
     * are header packets and are passed
     * back to caml through a callback. */
  } else {
    /* This second packet needs to
     * have the ogg header. */
    h->header_count++;
    if (h->header_count == 2) {
      op.packet = header;
      memcpy(op.packet + 13, buffer, bytes);
      op.bytes = bytes + 13;
      op.granulepos = 0;
      op.packetno = 0;
      op.b_o_s = 1;
      op.e_o_s = 0;
    } else {
      op.packet = (unsigned char *)buffer;
      op.bytes = bytes;
      op.granulepos = 0;
      h->packetno++;
      op.packetno = h->packetno;
      op.b_o_s = 0;
      op.e_o_s = 0;
    }

    if (h->header_count > 1) {
      ocaml_flac_register_thread();
      caml_acquire_runtime_system();

      value p = value_of_packet(&op);

      caml_register_generational_global_root(&p);

      value ret = caml_callback_exn(h->init_c, p);

      caml_remove_generational_global_root(&p);

      if (Is_exception_result(ret))
        caml_raise(Extract_exception(ret));

      caml_release_runtime_system();
    }
  }

  return FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
}

CAMLprim value ocaml_flac_encoder_ogg_create(value comments, value params,
                                             value os, value init_c) {
  CAMLparam4(comments, params, os, init_c);
  CAMLlocal2(tmp, ret);

  ret = ocaml_flac_encoder_alloc(comments, params, &ogg_encoder_ops);
  ocaml_flac_encoder *caml_enc = Encoder_val(ret);

  ocaml_flac_ogg_private *priv = malloc(sizeof(ocaml_flac_ogg_private));
  if (priv == NULL)
    caml_raise_out_of_memory();
  priv->data = NULL;
  priv->offset = 0;
  priv->bytes = 0;
  priv->granulepos = 0;
  priv->packetno = 0;
  priv->header_count = 0;
  priv->os = os;
  caml_register_generational_global_root(&priv->os);
  priv->init_c = init_c;
  caml_register_generational_global_root(&priv->init_c);

  caml_enc->callbacks.private = priv;

  caml_release_runtime_system();
  FLAC__stream_encoder_init_stream(caml_enc->encoder, ogg_enc_write_callback,
                                   NULL, NULL, NULL,
                                   (void *)&caml_enc->callbacks);
  caml_acquire_runtime_system();

  CAMLreturn(ret);
}

CAMLprim value ocaml_flac_encoder_ogg_finish(value e) {
  CAMLparam1(e);
  ocaml_flac_encoder *enc = Encoder_val(e);
  ocaml_flac_ogg_private *priv =
      (ocaml_flac_ogg_private *)enc->callbacks.private;
  ogg_stream_state *os = Stream_state_val(priv->os);
  ogg_packet op;

  op.packet = (unsigned char *)NULL;
  op.bytes = 0;
  op.b_o_s = 0;
  op.e_o_s = 1;
  op.granulepos = priv->granulepos + 1;
  op.packetno = priv->packetno + 1;

  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

/* Ogg skeleton interface */

/* Wrappers */
static void write32le(unsigned char *ptr, ogg_uint32_t v) {
  ptr[0] = v & 0xff;
  ptr[1] = (v >> 8) & 0xff;
  ptr[2] = (v >> 16) & 0xff;
  ptr[3] = (v >> 24) & 0xff;
}

static void write64le(unsigned char *ptr, ogg_int64_t v) {
  ogg_uint32_t hi = v >> 32;
  ptr[0] = v & 0xff;
  ptr[1] = (v >> 8) & 0xff;
  ptr[2] = (v >> 16) & 0xff;
  ptr[3] = (v >> 24) & 0xff;
  ptr[4] = hi & 0xff;
  ptr[5] = (hi >> 8) & 0xff;
  ptr[6] = (hi >> 16) & 0xff;
  ptr[7] = (hi >> 24) & 0xff;
}

/* Values from http://xiph.org/ogg/doc/skeleton.html */
#define FISBONE_IDENTIFIER "fisbone\0"
#define FISBONE_MESSAGE_HEADER_OFFSET 44
#define FISBONE_SIZE 52

/* Code from theorautils.c in ffmpeg2theora */
CAMLprim value ocaml_flac_skeleton_fisbone(value serial, value samplerate,
                                           value start, value content) {
  CAMLparam4(serial, samplerate, start, content);
  CAMLlocal1(packet);
  ogg_packet op;
  int len = FISBONE_SIZE + caml_string_length(content);

  memset(&op, 0, sizeof(op));
  op.packet = malloc(len);
  if (op.packet == NULL)
    caml_raise_out_of_memory();

  memset(op.packet, 0, len);
  /* it will be the fisbone packet for the vorbis audio */
  memcpy(op.packet, FISBONE_IDENTIFIER, 8); /* identifier */
  write32le(
      op.packet + 8,
      FISBONE_MESSAGE_HEADER_OFFSET); /* offset of the message header fields */
  write32le(op.packet + 12,
            Nativeint_val(serial)); /* serialno of the vorbis stream */
  write32le(op.packet + 16, 2);     /* number of header packet, 2 for now. */
  /* granulerate, temporal resolution of the bitstream in Hz */
  write64le(op.packet + 20,
            (ogg_int64_t)Int64_val(samplerate)); /* granulerate numerator */
  write64le(op.packet + 28, (ogg_int64_t)1);     /* granulerate denominator */
  write64le(op.packet + 36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet + 44, 2); /* preroll, for flac its 2 ??? */
  *(op.packet + 48) = 0;        /* granule shift, always 0 for flac */
  memcpy(op.packet + FISBONE_SIZE, String_val(content),
         caml_string_length(content));

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}
