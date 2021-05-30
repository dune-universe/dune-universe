#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include "gofw.h"

value caml_gofw_getSuspectp(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_double(gofw_Suspectp));
}

value caml_gofw_setSuspectp(value suspectp) {
  CAMLparam1(suspectp);
  gofw_Suspectp = Double_val(suspectp);
  CAMLreturn(Val_unit);
}
