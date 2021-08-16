#include "caml/mlvalues.h"
#include "caml/memory.h"

value caml_gt_hash_variant(value _str) {
  CAMLparam1(_str);
  CAMLlocal1(_ans);
  _ans = caml_hash_variant(String_val(_str));
  CAMLreturn(_ans);
}
