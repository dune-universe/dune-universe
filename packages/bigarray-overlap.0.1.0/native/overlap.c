#include "caml/bigarray.h"
#include "caml/memory.h"
#include "caml/alloc.h"

CAMLprim
value overlap_bytecode_caml_ba_ptr(value va)
{
  CAMLparam1(va);
  CAMLlocal1(res);

  struct caml_ba_array * a = Caml_ba_array_val(va);
  void *src_a = a->data;
  res = caml_copy_nativeint((intnat) src_a);

  CAMLreturn(res);
}

intnat
overlap_native_caml_ba_ptr(value va)
{
  struct caml_ba_array * a = Caml_ba_array_val(va);
  return ((intnat) a->data);
}
