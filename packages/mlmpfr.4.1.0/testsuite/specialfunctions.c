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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mpfr.h>

char *rounding_to_string (int r)
{
  char * buf = malloc (32);
  if (r == 0)
    {
      strcpy (buf, "Correct");
    }
  else if ( r < 0)
    {
      strcpy (buf, "Lower");
    }
  else
    {
      strcpy (buf, "Greater");
    }
  return buf;
}

void all (mpfr_t op1, mpfr_t op2)
{
  int ter;
  mpfr_t rop, sop, op22;
  mpfr_init (rop);
  mpfr_init (sop);
  mpfr_init (op22);

  ter = mpfr_log (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  if (mpfr_sgn (op1) >= 0)
    {
      ter = mpfr_log_ui (rop, (mpfr_get_si (op1, MPFR_RNDN)), MPFR_RNDN);
      mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
    }
  else
    {
      mpfr_set_nan (rop);
      mpfr_printf ("%.Re None\n", rop);
    }

  ter = mpfr_log2 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_log10 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_exp (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_exp2 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_exp10 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_sin (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_cos (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_tan (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_sin_cos (rop, sop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %.Re %s\n", rop, sop, ter == 0 ? rounding_to_string (ter) : "None");

  ter = mpfr_sec (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_csc (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_cot (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_acos (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_asin (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_atan (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_atan2 (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_sinh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_cosh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_tanh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_sinh_cosh (rop, sop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %.Re %s\n", rop, sop, ter == 0 ? rounding_to_string (ter) : "None");

  ter = mpfr_sech (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_csch (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_coth (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_acosh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_asinh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_atanh (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  if (mpfr_sgn (op1) >= 0)
    {
      ter = mpfr_fac_ui (rop, (mpfr_get_si (op1, MPFR_RNDN)), MPFR_RNDN);
      mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
    }
  else
    {
      mpfr_set_nan (rop);
      mpfr_printf ("%.Re None\n", rop);
    }

  ter = mpfr_log1p (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_expm1 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_eint (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_eint (rop, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_li2 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_gamma (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_gamma_inc (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_lngamma (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  int sign;
  ter = mpfr_lgamma (rop, &sign, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s %s\n", rop, sign > 0 ? "Positive" : "Negative", rounding_to_string (ter));

  ter = mpfr_digamma (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_zeta (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_beta (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_erf (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_erfc (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_j0 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_j1 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  mpfr_abs (op22, op2, MPFR_RNDN);
  ter = mpfr_jn (rop, mpfr_get_ui (op22, MPFR_RNDN), op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_y0 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_y1 (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_yn (rop, mpfr_get_ui (op22, MPFR_RNDN), op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_fma (rop, op1, op2, rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_fms (rop, op1, op2, rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_fmma (rop, op1, op2, rop, rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_fmms (rop, op1, op2, rop, rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_agm (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_hypot (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_ai (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_const_log2 (rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_const_pi (rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_const_euler (rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_const_catalan (rop, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  int size = 1000000;
  mpfr_ptr ptab [size];
  int i;
  for (i = 0; i < size; i+=2)
    {
      ptab[i] = op1;
      ptab[i+1] = op2;
    }
  ter = mpfr_sum (rop, ptab, size, MPFR_RNDN);
  mpfr_printf ("%.Re %s %d\n", rop, rounding_to_string (ter), size);
  ter = mpfr_dot (rop, ptab, ptab, size/10, MPFR_RNDN);
  mpfr_printf ("%.Re %s %d\n", rop, rounding_to_string (ter), size/10);

  mpfr_clear (rop);
  mpfr_clear (sop);
  mpfr_clear (op22);
}

int main ()
{
  mpfr_t op1, op2;
  mpfr_inits (op1, op2, NULL);

  mpfr_set_d (op1, 1. / 3, MPFR_RNDN);
  mpfr_set_d (op2, 1. / 10, MPFR_RNDN);

  all(op1, op2);
  printf("\n");

  mpfr_set_d (op1, -4. / 3, MPFR_RNDN);

  all(op1, op2);
  printf("\n");

  mpfr_set_d (op1, 1. / 3, MPFR_RNDN);
  mpfr_set_d (op2, -113. / 10, MPFR_RNDN);

  all(op1, op2);
  printf("\n");

  mpfr_set_d (op1, +0, MPFR_RNDN);
  mpfr_set_d (op2, -1. / 10, MPFR_RNDN);

  all(op1, op2);
  printf("\n");

  mpfr_set_d (op1, -0., MPFR_RNDN);
  mpfr_set_d (op2, -0., MPFR_RNDN);

  all(op1, op2);

  mpfr_clears (op1, op2, NULL);
  return 0;
}
