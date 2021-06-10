#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include "swrite.h"

value caml_swrite_getBasic(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Basic == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setBasic(value b) {
  CAMLparam1(b);
  swrite_Basic = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}

value caml_swrite_getParameters(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Parameters == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setParameters(value b) {
  CAMLparam1(b);
  swrite_Parameters = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}

value caml_swrite_getCollectors(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Collectors == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setCollectors(value b) {
  CAMLparam1(b);
  swrite_Collectors = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}

value caml_swrite_getClasses(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Classes == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setClasses(value b) {
  CAMLparam1(b);
  swrite_Classes = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}

value caml_swrite_getCounters(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Counters == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setCounters(value b) {
  CAMLparam1(b);
  swrite_Counters = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}

value caml_swrite_getHost(value unit) {
  CAMLparam1(unit);
  CAMLreturn((swrite_Host == TRUE) ? Val_true : Val_false);
}

value caml_swrite_setHost(value b) {
  CAMLparam1(b);
  swrite_Host = (Bool_val(b) ? TRUE : FALSE);
  CAMLreturn(Val_unit);
}
