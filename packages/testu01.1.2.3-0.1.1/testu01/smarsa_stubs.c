#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include "unif01.h"
#include "sres.h"
#include "smarsa.h"
#include "unif01_stubs.h"
#include "sres_stubs.h"

value caml_smarsa_BirthdaySpacings(value bgen, value obpoisson, value N, value n,
                                   value r, value d, value t, value Order) {
  CAMLparam5(bgen, obpoisson, N, n, r); CAMLxparam3(d, t, Order);

  unif01_Gen* gen = unif01_Gen_unbox(bgen);

  sres_Poisson* poisson = NULL;

  if (Is_block(obpoisson)) { /* if obpoisson = Some ... */
    value bpoisson = Field(obpoisson, 1);
    poisson = sres_Poisson_unbox(bpoisson);
  }

  smarsa_BirthdaySpacings(gen, poisson, Long_val(N), Long_val(n), Int_val(r),
                          Long_val(d), Int_val(t), Int_val(Order));

  CAMLreturn(Val_unit);
}
value camlbytecode_smarsa_BirthdaySpacings(value* argv, int argn) {
  return caml_smarsa_BirthdaySpacings(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}
