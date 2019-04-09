
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "crlibm.h"

static unsigned long long cw;

CAMLexport value caml_crlibm_init(value unit)
{
  cw = crlibm_init();
  return Val_unit;
}

CAMLexport value caml_crlibm_exit(value unit)
{
  crlibm_exit(cw);
  return Val_unit;
}


#define WRAP1(fn)                                       \
  CAMLexport value fn##_bc(value x) {                   \
    return(caml_copy_double(fn(Double_val(x))));        \
  }

#define WRAP1_4ROUNDING(name) \
  WRAP1(name##_rn)            \
  WRAP1(name##_rd)            \
  WRAP1(name##_ru)            \
  WRAP1(name##_rz)

WRAP1(exp_rn)
WRAP1(exp_rd)
WRAP1(exp_ru)

WRAP1_4ROUNDING(log)
WRAP1_4ROUNDING(cos)
WRAP1_4ROUNDING(sin)
WRAP1_4ROUNDING(tan)
WRAP1_4ROUNDING(cospi)
WRAP1_4ROUNDING(sinpi)
WRAP1_4ROUNDING(tanpi)

WRAP1_4ROUNDING(atan)
WRAP1_4ROUNDING(atanpi)
WRAP1_4ROUNDING(cosh)
WRAP1_4ROUNDING(sinh)
WRAP1_4ROUNDING(log2)
WRAP1_4ROUNDING(log10)
WRAP1_4ROUNDING(asin)
WRAP1(acos_rn)
WRAP1(acos_rd)
WRAP1(acos_ru)
WRAP1_4ROUNDING(asinpi)
WRAP1(acospi_rn)
WRAP1(acospi_rd)
WRAP1(acospi_ru)
WRAP1_4ROUNDING(expm1)
WRAP1_4ROUNDING(log1p)

CAMLexport value pow_rn_bc(value x, value y) {
  return(caml_copy_double(pow_rn(Double_val(x), Double_val(y))));
}
