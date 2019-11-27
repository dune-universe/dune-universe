#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value get_success_code(value unit) {
  CAMLparam1(unit);
  return Val_int(EXIT_SUCCESS);
}

CAMLprim value get_failure_code(value unit) {
  CAMLparam1(unit);
  return Val_int(EXIT_FAILURE);
}
