#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include "unif01.h"
#include "usoft.h"
#include "unif01_stubs.h"

void usoft_unif01_Gen_boxed_finalize(value bgen) {
  usoft_DeleteGen(unif01_Gen_unbox(bgen));
}

static struct custom_operations usoft_unif01_Gen_boxed = {
  .identifier = "fr.boloss.testu01.usoft.unif01_Gen",
  .finalize = usoft_unif01_Gen_boxed_finalize,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default
};

value caml_usoft_CreateJava48(value s, value jflag) {
  CAMLparam2(s, jflag);
  CAMLlocal1(bgen);
  unif01_Gen* gen = usoft_CreateJava48(Long_val(s), Int_val(jflag));
  unif01_Gen_box(gen, bgen, usoft_unif01_Gen_boxed);
  CAMLreturn(bgen);
}
