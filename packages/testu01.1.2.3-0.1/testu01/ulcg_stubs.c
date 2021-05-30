#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include "unif01.h"
#include "ulcg.h"
#include "unif01_stubs.h"

void ulcg_unif01_Gen_boxed_finalize(value bgen) {
  ulcg_DeleteGen(unif01_Gen_unbox(bgen));
}

static struct custom_operations ulcg_unif01_Gen_boxed = {
 .identifier = "fr.boloss.testu01.ulcg.unif01_Gen",
 .finalize = ulcg_unif01_Gen_boxed_finalize,
 .compare = custom_compare_default,
 .hash = custom_hash_default,
 .serialize = custom_serialize_default,
 .deserialize = custom_deserialize_default
};

value caml_ulcg_CreateLCG(value m, value a, value c, value s) {
  CAMLparam4(m, a, c, s);
  CAMLlocal1(bgen);
  unif01_Gen* gen = ulcg_CreateLCG(Long_val(m), Long_val(a), Long_val(c), Long_val(s));
  unif01_Gen_box(gen, bgen, ulcg_unif01_Gen_boxed);
  CAMLreturn(bgen);
}
