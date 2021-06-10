#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include "sres.h"
#include "sres_stubs.h"

void finalize_sres_Poisson_boxed(value bpoisson) {
  sres_DeletePoisson(sres_Poisson_unbox(bpoisson));
  return;
}

static struct custom_operations sres_Poisson_boxed = {
 .identifier = "fr.boloss.testu01.sres.sres_Poisson",
 .finalize = finalize_sres_Poisson_boxed,
 .compare = custom_compare_default,
 .hash = custom_hash_default,
 .serialize = custom_serialize_default,
 .deserialize = custom_deserialize_default
};

value caml_sres_CreatePoisson(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(bpoisson);
  sres_Poisson* poisson = sres_CreatePoisson();
  sres_Poisson_box(poisson, bpoisson, sres_Poisson_boxed);
  CAMLreturn(Val_unit);
}
