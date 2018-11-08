#include "checkseum.h"
#include "adler32.h"
#include "crc32c.h"
#include "crc32.h"

#include <stdio.h>

#ifdef ARCH_SIXTYFOUR
/* XXX(dinosaure): un-allocated version for 64-bits architecture. */

CAMLprim value
caml_checkseum_adler32_ba(value t, value src, value off, value len)
{
  return (Val_int (adler32(Int_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_adler32_st(value t, value src, value off, value len)
{
  return (Val_int (adler32(Int_val (t), _st_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32c_ba(value t, value src, value off, value len)
{
  return (Val_int (crc32c(Int_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32c_st(value t, value src, value off, value len)
{
  return (Val_int (crc32c(Int_val (t), _st_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32_ba(value t, value src, value off, value len)
{
  return (Val_int (crc32(Int_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32_st(value t, value src, value off, value len)
{
  return (Val_int (crc32(Int_val (t), _st_uint8_off (src, off), Int_val (len))));
}

#else
/* XXX(dinosaure): allocated version for 32-bits architecture. */

CAMLprim value
caml_checkseum_adler32_ba(value t, value src, value off, value len)
{
  return (copy_int32(adler32(Int32_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_adler32_st(value t, value src, value off, value len)
{
  return (copy_int32(adler32(Int32_val (t), _st_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32c_ba(value t, value src, value off, value len)
{
  return (copy_int32(crc32c(Int32_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32c_st(value t, value src, value off, value len)
{
  return (copy_int32(crc32c(Int32_val (t), _st_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32_ba(value t, value src, value off, value len)
{
  return (copy_int32(crc32(Int32_val (t), _ba_uint8_off (src, off), Int_val (len))));
}

CAMLprim value
caml_checkseum_crc32_st(value t, value src, value off, value len)
{
  return (copy_int32(crc32(Int32_val (t), _st_uint8_off (src, off), Int_val (len))));
}

#endif
