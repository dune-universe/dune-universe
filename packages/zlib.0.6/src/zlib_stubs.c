/*
 * Copyright (c) 2015, Christopher Zimmermann
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <zlib.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/misc.h>


value zlib_adler32(value vadler, value vbuf)
{
  return caml_copy_int32(
      adler32(
	Int32_val(vadler),
	(Bytef *)String_val(vbuf),
	caml_string_length(vbuf)));
}

value zlib_error(z_streamp strm, int error)
{
  CAMLparam0();
  CAMLlocal1(v);
  char buf[128];

  switch (error)
  {
    case Z_OK:		/* 0 */
    case Z_STREAM_END:	/* 1 */
    case Z_NEED_DICT:	/* 2 */
      CAMLreturn(Val_int(error));
    case Z_BUF_ERROR:
      CAMLreturn(Val_int(3));
    case Z_DATA_ERROR:
      v = caml_alloc_small(1, 0);
      Field(v, 0) = Val_unit;
      Field(v, 0) = caml_copy_string(strm->msg ? strm->msg : "");
      CAMLreturn(v);
    case Z_VERSION_ERROR:
      snprintf(buf, sizeof(buf), "Zlib version error: %s",
	  strm->msg ? strm->msg : "");
      caml_failwith(buf);
      break;
    case Z_MEM_ERROR:
      caml_raise_out_of_memory();
      break;
    case Z_STREAM_ERROR:
      snprintf(buf, sizeof(buf), "Zlib stream error: %s",
	  strm->msg ? strm->msg : "");
      caml_invalid_argument(buf);
      break;
    case Z_ERRNO:
      /* strerror is not thread-safe,
       * but that's no problem since we won't context-switch here. */
      caml_failwith(strerror(errno));
      break;
    default:
      caml_failwith("Zlib: Unknown return code");
      break;
  }
  /* not reached */
  CAMLassert(0);
}

struct wrap_strm {
  z_streamp strm;
  gz_headerp header;
  int flags;
};
#define ZLIB_INFLATE 1
#define WRAP_STRM_WOSIZE \
  ((sizeof(struct wrap_strm) + sizeof(value) - 1) / sizeof(value))

void zlib_finalize(value vwrap)
{
  struct wrap_strm *wrap = Data_custom_val(vwrap);
  int ret;

  if (wrap->flags & ZLIB_INFLATE)
    ret = inflateEnd(wrap->strm);
  else
    ret = deflateEnd(wrap->strm);

  CAMLassert(ret == Z_OK || ret == Z_DATA_ERROR);
  caml_stat_free(wrap->strm);
  caml_stat_free(wrap->header);
}

#define ZLIB_MAX_MEMORY 1024*1024

CAMLprim value zlib_deflate_init(
    value level,
    value method,
    value windowBits,
    value memLevel,
    value strategy)
{
  CAMLparam0(); /* has only immediate arguments */
  CAMLlocal1(vwrap);
  struct wrap_strm *wrap;
  z_streamp strm;
  size_t memory;

  CAMLassert(Is_long(strategy) && Is_long(method) && 0 == Long_val(method));

  strm = caml_stat_alloc(sizeof(z_stream));
  memset(strm, 0, sizeof(z_stream));

  /* From zconf.h:
     The memory requirements for deflate are (in bytes):
	      (1 << (windowBits+2)) +  (1 << (memLevel+9))
   that is: 128K for windowBits=15  +  128K for memLevel = 8  (default values)
   plus a few kilobytes for small objects. For example, if you want to reduce
   the default memory requirements from 256K to 128K, compile with
       make CFLAGS="-O -DMAX_WBITS=14 -DMAX_MEM_LEVEL=7"
   Of course this will generally degrade compression (there's no free lunch).

     The memory requirements for inflate are (in bytes) 1 << windowBits
   that is, 32K for windowBits=15 (default value) plus a few kilobytes
   for small objects.
  */
  memory =  1 << ((abs(Int_val(windowBits)) & 15) + 2);
  memory += 1 << (Int_val(memLevel) + 9);
  vwrap = caml_alloc_final(WRAP_STRM_WOSIZE, zlib_finalize, memory, ZLIB_MAX_MEMORY);
  wrap = Data_custom_val(vwrap);
  wrap->strm = strm;
  wrap->header = NULL;
  wrap->flags = 0;

  zlib_error(strm,
      deflateInit2(strm,
	Int_val(level),
	Z_DEFLATED,
	Int_val(windowBits),
	Int_val(memLevel),
	Int_val(strategy)));

  CAMLreturn(vwrap);
}

CAMLprim value zlib_inflate_init(value windowBits)
{
  CAMLparam0(); /* has only immediate arguments */
  CAMLlocal1(vwrap);
  struct wrap_strm *wrap;
  z_streamp strm;
  gz_headerp header = NULL;
  size_t memory = 0;
  const int wBits = Int_val(windowBits);

  /* prepare gz header struct for gz and automatic header detect mode */
  if (wBits > 15) {
    const size_t extral = 4096, namel = 512, commentl = 4096;
    const size_t totall = sizeof(gz_header) + extral + namel + commentl;

    header = caml_stat_alloc(totall);
    memory += totall;

    header->extra	= (Bytef *)header + sizeof(gz_header);
    header->name	= header->extra + extral;
    header->comment	= header->name + namel;
    header->extra_max	= extral;
    header->name_max 	= namel;
    header->comm_max 	= commentl;
    header->hcrc	= 0;
    header->done	= 0;
  }

  memory += abs(wBits) & 15 ? 1 << (abs(wBits) & 15) : 1 << 15;
  vwrap = caml_alloc_final(WRAP_STRM_WOSIZE, zlib_finalize, memory, ZLIB_MAX_MEMORY);
  wrap = Data_custom_val(vwrap);

  wrap->flags = ZLIB_INFLATE;
  wrap->header = header;
  wrap->strm = strm = caml_stat_alloc(sizeof(z_stream));
  memset(strm, 0, sizeof(z_stream));

  zlib_error(strm, inflateInit2(strm, wBits));
  if (header != NULL)
    zlib_error(strm, inflateGetHeader(strm, header));

  CAMLreturn(vwrap);
}

CAMLprim value zlib_deflate_bound(value vwrap, value len)
{
  CAMLparam1(vwrap); /* len is an immediate. */
  struct wrap_strm *wrap = Data_custom_val(vwrap);
  int ret;

  CAMLassert((wrap->flags & ZLIB_INFLATE) == 0);

  ret = deflateBound(wrap->strm, Long_val(len));

  if (ret < 0)
    caml_failwith("Zlib.deflate_bound");
  else
    CAMLreturn(Val_long(ret));
}

CAMLprim value zlib_reset(value vstrm)
{
  CAMLparam1(vstrm);
  int ret;

  struct wrap_strm *wrap = Data_custom_val(Field(vstrm,0));
  z_streamp strm = wrap->strm;
  gz_headerp header = wrap->header;

  if (wrap->flags & ZLIB_INFLATE) {
    ret = inflateReset(strm);
    if (header != NULL)
      zlib_error(strm, inflateGetHeader(strm, header));
  }
  else
    ret = deflateReset(strm);

  Field(vstrm, 7) = Val_long(strm->total_in);
  Field(vstrm, 8) = Val_long(strm->total_out);
  Field(vstrm, 9) = Val_long(Z_UNKNOWN);
  Store_field(vstrm,10, caml_copy_int32(strm->adler));
  Store_field(vstrm,10, caml_copy_int32(strm->adler));

  zlib_error(strm, ret);

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_deflate_set_dictionary(value vstrm, value vdict)
{
  CAMLparam2(vstrm, vdict);
  struct wrap_strm *wrap = Data_custom_val(vstrm);
  z_streamp strm = wrap->strm;

  CAMLassert((wrap->flags & ZLIB_INFLATE) == 0);

  zlib_error(strm,
      deflateSetDictionary(strm,
	(Bytef *)String_val(vdict),
	caml_string_length(vdict)));

  CAMLreturn(caml_copy_int32(strm->adler));
}

CAMLprim value zlib_inflate_set_dictionary(value vstrm, value vdict)
{
  CAMLparam2(vstrm, vdict);
  struct wrap_strm *wrap = Data_custom_val(vstrm);
  z_streamp strm = wrap->strm;

  CAMLassert(wrap->flags & ZLIB_INFLATE);

  CAMLreturn(
    zlib_error(strm,
	inflateSetDictionary(strm,
	  (Bytef *)String_val(vdict),
	  caml_string_length(vdict))));
}

CAMLprim value zlib_set_header(value vstrm, value vheader)
{
  CAMLparam2(vstrm, vheader);
  struct wrap_strm *wrap = Data_custom_val(vstrm);
  z_streamp strm = wrap->strm;
  gz_headerp header;
  Bytef *p;
  size_t len = sizeof(gz_header);

  CAMLassert((wrap->flags & ZLIB_INFLATE) == 0);

  /* extra */
  if (Is_block(Field(vheader,4))) {
    CAMLassert(Tag_val(Field(vheader,4)) == 0);
    /* zlib does *not* expect this string to be zero-terminated */
    len += caml_string_length(Field(Field(vheader,4),0));
  }
  else
    CAMLassert(Int_val(Field(vheader,4)) == 0);

  /* name */
  if (Is_block(Field(vheader,5))) {
    CAMLassert(Tag_val(Field(vheader,5)) == 0);
    /* zlib *does* expect this string to be zero-terminated - add one. */
    len += caml_string_length(Field(Field(vheader,5),0)) + 1;
  }
  else
    CAMLassert(Int_val(Field(vheader,5)) == 0);

  /* comment */
  if (Is_block(Field(vheader,6))) {
    CAMLassert(Tag_val(Field(vheader,6)) == 0);
    /* zlib *does* expect this string to be zero-terminated - add one. */
    len += caml_string_length(Field(Field(vheader,6),0)) + 1;
  }
  else
    CAMLassert(Int_val(Field(vheader,6)) == 0);

  if (wrap->header != NULL)
    caml_stat_free(wrap->header);
  wrap->header = header = caml_stat_alloc(len);

  memset(header, 0, sizeof(gz_header));
  header->text =    Int_val(Field(vheader,0));
  header->time = Int32_val(Field(vheader,1));
  header->os   =   Long_val(Field(vheader,2));
  p = (Bytef *)header + sizeof(gz_header);

  /* extra */
  if (Is_block(Field(vheader,4))) {
    /* zlib does *not* expect this string to be zero-terminated */
    len = caml_string_length(Field(Field(vheader,4),0));
    memcpy(p, String_val(Field(Field(vheader,4),0)), len);
    header->extra_len = len;
    header->extra = p;
    p += len;
  }

  /* name */
  if (Is_block(Field(vheader,5))) {
    /* zlib *does* expect this string to be zero-terminated - add one.
     * OCaml strings are zery-terminated, just copy the final '\0', too. */
    len = caml_string_length(Field(Field(vheader,5),0)) + 1;
    memcpy(p, String_val(Field(Field(vheader,5),0)), len);
    header->name = p;
    p += len;
  }

  /* comment */
  if (Is_block(Field(vheader,6))) {
    /* zlib *does* expect this string to be zero-terminated - add one.
     * OCaml strings are zery-terminated, just copy the final '\0', too. */
    len = caml_string_length(Field(Field(vheader,6),0)) + 1;
    memcpy(p, String_val(Field(Field(vheader,6),0)), len);
    header->comment = p;
    p += len;
  }

  zlib_error(strm,
      deflateSetHeader(strm, header));

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_get_header(value vstrm)
{
  CAMLparam1(vstrm);
  CAMLlocal5(vheader, extra, comment, name, tmp);
  struct wrap_strm *wrap = Data_custom_val(vstrm);
  gz_headerp header = wrap->header;
  int len;

  CAMLassert(wrap->flags & ZLIB_INFLATE);

  /* not in gzip or auto mode or zlib header found */
  if (header == NULL || header->done == -1)
    caml_raise_not_found();
  /* header not yet completed */
  if (header->done == 0)
    caml_failwith("Zlib.get_header: Header not yet completed.");
  CAMLassert(header->done == 1);

  if (header->extra != NULL) {
    len = header->extra_len < header->extra_max
	? header->extra_len : header->extra_max;
    tmp = caml_alloc_string(len);
    memcpy(String_val(tmp), header->extra, len);
    extra = caml_alloc_small(1, 0);
    Field(extra,0) = tmp;
  }
  else
    extra = Val_int(0);

  if (header->name != NULL) {
    len = strnlen((char *)header->name, header->name_max);
    tmp = caml_alloc_string(len);
    memcpy(String_val(tmp), header->name, len);
    name = caml_alloc_small(1, 0);
    Field(name,0) = tmp;
  }
  else
    name = Val_int(0);

  if (header->comment != NULL) {
    len = strnlen((char *)header->comment, header->comm_max);
    tmp = caml_alloc_string(len);
    memcpy(String_val(tmp), header->comment, len);
    comment = caml_alloc_small(1, 0);
    Field(comment,0) = tmp;
  }
  else
    comment = Val_int(0);

  tmp = caml_copy_int32(header->time);

  vheader = caml_alloc_small(7, 0);
  Field(vheader,0) = Val_int(header->text);
  Field(vheader,1) = tmp;
  Field(vheader,2) = Val_int(header->os);
  Field(vheader,3) = Val_int(header->xflags);
  Field(vheader,4) = extra;
  Field(vheader,5) = name;
  Field(vheader,6) = comment;

  CAMLreturn(vheader);
}

CAMLprim value zlib_flate(value vstrm, value flush)
{
  CAMLparam2(vstrm, flush);
  int ret;
  struct wrap_strm *wrap = Data_custom_val(Field(vstrm,0));
  z_streamp strm = wrap->strm;

  CAMLassert(Is_long(flush));
  CAMLassert(Caml_ba_array_val(Field(vstrm, 1))->num_dims == 1);

# define LField(n) Long_val(Field(vstrm,(n)))

  /* By default use whole bigarray */
  strm->avail_in   = Caml_ba_array_val(Field(vstrm, 1))->dim[0];
  strm->avail_out  = Caml_ba_array_val(Field(vstrm, 2))->dim[0];
  /* if offset is given, trim length accordingly. */
  strm->avail_in  -= LField(3);
  strm->avail_out -= LField(4);
  /* given substring inside bigarray bounds? */
  if (LField(3) < 0 || LField(4) < 0 ||
      LField(5) > strm->avail_in || LField(6) > strm->avail_out)
    caml_invalid_argument("Zlib.flate");
  /* if given length is negative use default and return it. */
  if (LField(5) < 0)
    Field(vstrm,5)  = Val_long(strm->avail_in);
  else
    strm->avail_in  = LField(5);
  if (LField(6) < 0)
    Field(vstrm,6)  = Val_long(strm->avail_out);
  else
    strm->avail_out = LField(6);

  CAMLassert(strm->avail_in == LField(5));
  CAMLassert(strm->avail_out == LField(6));

  strm->next_in   = (Byte *)Caml_ba_data_val(Field(vstrm, 1)) + LField(3);
  strm->next_out  = (Byte *)Caml_ba_data_val(Field(vstrm, 2)) + LField(4);

  if (wrap->flags & ZLIB_INFLATE) {
    caml_release_runtime_system();
    ret = inflate(strm, Int_val(flush));
  }
  else {
    caml_release_runtime_system();
    ret = deflate(strm, Int_val(flush));
  }
  caml_acquire_runtime_system();

  /* Long overwriting long can be assigned directly without caml_modify */
  Field(vstrm, 3) = Val_long(LField(3) + (LField(5) - strm->avail_in));
  Field(vstrm, 4) = Val_long(LField(4) + (LField(6) - strm->avail_out));
  Field(vstrm, 5) = Val_long(strm->avail_in);
  Field(vstrm, 6) = Val_long(strm->avail_out);
  Field(vstrm, 7) = Val_long(strm->total_in);
  Field(vstrm, 8) = Val_long(strm->total_out);
  Field(vstrm, 9) = Val_long(strm->data_type);
  Store_field(vstrm,10, caml_copy_int32(strm->adler));

  CAMLreturn(zlib_error(strm, ret));
}
