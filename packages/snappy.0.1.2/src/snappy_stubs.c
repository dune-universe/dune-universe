
#include <assert.h>
#include <memory.h>
#include <snappy.h>

extern "C" {

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

static void raise_error(char const* s)
{
  static const value* exn = NULL;
  if (NULL == exn)
  {
    exn = caml_named_value("Snappy.Error");
    assert(NULL != exn);
  }
  caml_raise_with_string(*exn, s);
}

CAMLprim value caml_snappy_compress(value v_s, value v_ofs, value v_len)
{
  CAMLparam3(v_s, v_ofs, v_len);
  CAMLlocal1(v_out);

  char* buf = NULL;
  try
  {
    buf = (char*)caml_stat_alloc(snappy::MaxCompressedLength(Int_val(v_len)));
    size_t out_len = 0;
    snappy::RawCompress(&Byte(v_s, Int_val(v_ofs)), Int_val(v_len), buf, &out_len);
    v_out = caml_alloc_string(out_len);
    memcpy(&Byte(v_out,0), buf, out_len);
    caml_stat_free(buf);
  }
  catch (std::exception const& e)
  {
    if (NULL != buf) caml_stat_free(buf);
    raise_error(e.what());
  }

  CAMLreturn(v_out);
}

CAMLprim value caml_snappy_uncompress(value v_s, value v_ofs, value v_len)
{
  CAMLparam3(v_s,v_ofs,v_len);
  CAMLlocal1(v_out);

  try
  {
    size_t out_len = 0;
    if (!snappy::GetUncompressedLength(&Byte(v_s, Int_val(v_ofs)), Int_val(v_len), &out_len))
    {
      raise_error("GetUncompressedLength failed");
    }
    v_out = caml_alloc_string(out_len);
    if (!snappy::RawUncompress(&Byte(v_s, Int_val(v_ofs)), Int_val(v_len), &Byte(v_out,0)))
    {
      raise_error("RawUncompress failed");
    }
  }
  catch (std::exception const& e)
  {
    raise_error(e.what());
  }

  CAMLreturn(v_out);
}

CAMLprim value caml_snappy_is_valid(value v_s, value v_ofs, value v_len)
{
  CAMLparam3(v_s,v_ofs,v_len);
  bool ok = false;

  try
  {
    ok = snappy::IsValidCompressedBuffer(&Byte(v_s, Int_val(v_ofs)), Int_val(v_len));
  }
  catch (std::exception const& e)
  {
    raise_error(e.what());
  }

  CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_snappy_get_u_size(value v_s, value v_ofs, value v_len)
{
  CAMLparam3(v_s,v_ofs,v_len);
  size_t out_len = 0;

  try
  {
    if (!snappy::GetUncompressedLength(&Byte(v_s, Int_val(v_ofs)), Int_val(v_len), &out_len))
    {
      raise_error("GetUncompressedLength failed");
    }
    if (out_len > Max_long)
    {
      raise_error("GetUncompressedLength overflow");
    }
  }
  catch (std::exception const& e)
  {
    raise_error(e.what());
  }

  CAMLreturn(Val_int(out_len));
}

} // extern "C"

