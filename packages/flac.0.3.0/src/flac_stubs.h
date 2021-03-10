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

#include <pthread.h>

#include <FLAC/format.h>
#include <FLAC/metadata.h>
#include <FLAC/stream_decoder.h>
#include <FLAC/stream_encoder.h>

#include <caml/mlvalues.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
value flac_Val_some(value v);

/* Decoder */

typedef struct ocaml_flac_decoder_callbacks {
  /* This is used for ogg callbacks. */
  void *private;
  /* This is used for callback from caml. */
  value data;
  value read;
  value seek;
  value tell;
  value length;
  value eof;
  value write;
  FLAC__StreamMetadata_StreamInfo *info;
  FLAC__StreamMetadata *meta;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

#define Fill_dec_values(x, c)                                                  \
  {                                                                            \
    caml_modify_generational_global_root(&(x->callbacks.read), Field(c, 0));   \
    caml_modify_generational_global_root(&(x->callbacks.seek), Field(c, 1));   \
    caml_modify_generational_global_root(&(x->callbacks.tell), Field(c, 2));   \
    caml_modify_generational_global_root(&(x->callbacks.length), Field(c, 3)); \
    caml_modify_generational_global_root(&(x->callbacks.eof), Field(c, 4));    \
    caml_modify_generational_global_root(&(x->callbacks.write), Field(c, 5));  \
  }
#define Free_dec_values(x)                                                     \
  {                                                                            \
    caml_modify_generational_global_root(&(x->callbacks.read), Val_unit);      \
    caml_modify_generational_global_root(&(x->callbacks.seek), Val_unit);      \
    caml_modify_generational_global_root(&(x->callbacks.tell), Val_unit);      \
    caml_modify_generational_global_root(&(x->callbacks.length), Val_unit);    \
    caml_modify_generational_global_root(&(x->callbacks.eof), Val_unit);       \
    caml_modify_generational_global_root(&(x->callbacks.write), Val_unit);     \
  }

value ocaml_flac_decoder_alloc(struct custom_operations *decoder_ops);

void finalize_decoder(value dec);

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder **)Data_custom_val(v)))

void dec_metadata_callback(const FLAC__StreamDecoder *decoder,
                           const FLAC__StreamMetadata *metadata,
                           void *client_data);

FLAC__StreamDecoderWriteStatus
dec_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame,
                   const FLAC__int32 *const buffer[], void *client_data);

void dec_error_callback(const FLAC__StreamDecoder *decoder,
                        FLAC__StreamDecoderErrorStatus status,
                        void *client_data);

/* Encoder */

typedef struct ocaml_flac_encoder_callbacks {
  /* This is used by the caml encoder. */
  value write;
  value seek;
  value tell;
  /* This is used by the ogg encoder. */
  void *private;
} ocaml_flac_encoder_callbacks;

typedef struct ocaml_flac_encoder {
  FLAC__StreamEncoder *encoder;
  FLAC__StreamMetadata *meta;
  FLAC__int32 **buf;
  FLAC__int32 *lines;
  ocaml_flac_encoder_callbacks callbacks;
} ocaml_flac_encoder;

#define Fill_enc_values(x, c)                                                  \
  {                                                                            \
    caml_modify_generational_global_root(&(x->callbacks.write), Field(c, 0));  \
    caml_modify_generational_global_root(&(x->callbacks.seek), Field(c, 1));   \
    caml_modify_generational_global_root(&(x->callbacks.tell), Field(c, 2));   \
  }
#define Free_enc_values(x)                                                     \
  {                                                                            \
    caml_modify_generational_global_root(&(x->callbacks.write), Val_unit);     \
    caml_modify_generational_global_root(&(x->callbacks.seek), Val_unit);      \
    caml_modify_generational_global_root(&(x->callbacks.tell), Val_unit);      \
  }

/* Caml abstract value containing the decoder. */
#define Encoder_val(v) (*((ocaml_flac_encoder **)Data_custom_val(v)))

value ocaml_flac_encoder_alloc(value comments, value params,
                               struct custom_operations *encoder_ops);

void finalize_encoder(value dec);

/* Threads management */
void ocaml_flac_register_thread();
