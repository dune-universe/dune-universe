/* This file is part of mlmpfr.

  mlmpfr is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  mlmpfr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with mlmpfr. If not, see
 <http://www.gnu.org/licenses/>. */

#include <stdio.h>
#include <mpfr.h>

#define PRINTM(X)  mpfr_printf ("%.Re\n", X)

#define INIT(X)    mpfr_t X; mpfr_init (X)
#define CLEAR(X)   mpfr_clear (X)

#define NAN(X)     mpfr_set_nan (X); PRINTM (X)

#define M_TO_SI(X) mpfr_get_si (X, MPFR_RNDN)
#define M_TO_UI(X) mpfr_get_ui (X, MPFR_RNDN)
#define M_TO_D(X)  mpfr_get_d  (X, MPFR_RNDN)

#define UNAOP_M(OP, X, Z)  mpfr_##OP      (Z, X,           MPFR_RNDN); PRINTM (Z)
#define UNAOP_UI(OP, X, Z) mpfr_##OP##_ui (Z, M_TO_UI (X), MPFR_RNDN); PRINTM (Z)

#define BINOP_MM(OP, X, Y, Z)  mpfr_##OP      (Z, X,           Y,           MPFR_RNDN); PRINTM (Z)
#define BINOP_MSI(OP, X, Y, Z) mpfr_##OP##_si (Z, X,           M_TO_SI (Y), MPFR_RNDN); PRINTM (Z)
#define BINOP_MD(OP, X, Y, Z)  mpfr_##OP##_d  (Z, X,           M_TO_D (Y),  MPFR_RNDN); PRINTM (Z)
#define BINOP_SIM(OP, X, Y, Z) mpfr_si_##OP   (Z, M_TO_SI (X), Y,           MPFR_RNDN); PRINTM (Z)
#define BINOP_DM(OP, X, Y, Z)  mpfr_d_##OP    (Z, M_TO_D (X),  Y,           MPFR_RNDN); PRINTM (Z)

void
all (mpfr_t op1, mpfr_t op2)
{
  INIT (rop);

  BINOP_MM  (add, op1, op2, rop);
  BINOP_MSI (add, op1, op2, rop);
  BINOP_MD  (add, op1, op2, rop);

  BINOP_MM  (sub, op1, op2, rop);
  BINOP_MSI (sub, op1, op2, rop);
  BINOP_SIM (sub, op1, op2, rop);
  BINOP_MD  (sub, op1, op2, rop);
  BINOP_DM  (sub, op1, op2, rop);

  BINOP_MM  (mul, op1, op2, rop);
  BINOP_MSI (mul, op1, op2, rop);
  BINOP_MD  (mul, op1, op2, rop);

  BINOP_MM  (div, op1, op2, rop);
  BINOP_MSI (div, op1, op2, rop);
  BINOP_SIM (div, op1, op2, rop);
  BINOP_MD  (div, op1, op2, rop);
  BINOP_DM  (div, op1, op2, rop);

  UNAOP_M   (sqrt, op1, rop);
  if (mpfr_sgn (op1) < 0) {
    NAN (rop);
  } else {
    UNAOP_UI (sqrt, op1, rop);
  }

  UNAOP_M   (cbrt, op1, rop);

  mpfr_rootn_ui (rop, op1, 10, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_pow (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);
  mpfr_pow_si (rop, op1, mpfr_get_si (op2, MPFR_RNDN), MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_neg (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_abs (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_dim (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_mul_2si (rop, op1, mpfr_get_si (op2, MPFR_RNDN), MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_div_2si (rop, op1, mpfr_get_si (op2, MPFR_RNDN), MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_clear (rop);
}

#define SETD(X, V) mpfr_set_d (X, V, MPFR_RNDN)

#define RUN(X, Y) {                                             \
    INIT (op1);     INIT (op2);                                 \
    SETD (op1, X);  SETD (op2, Y);                              \
    all (op1, op2); printf ("\n");                              \
    CLEAR (op1);    CLEAR (op2);                                \
  }

int main ()
{
  RUN (1. / 3,  1. / 10);
  RUN (-4. / 3, 1. / 10);
  RUN (1. / 3,  -113. / 10);
  RUN (+0.,     -1. / 10);
  RUN (-0.,     -0.);

  return 0;
}
