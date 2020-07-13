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
#include <math.h>
#include <stdlib.h>

void all (mpfr_t op1, mpfr_t op2)
{
  mpfr_t rop;
  mpfr_init (rop);

  printf("%d\n", mpfr_cmp (op1, op2));
  printf("%d\n", mpfr_cmp_si (op1, mpfr_get_si (op2, MPFR_RNDN)));
  printf("%d\n", mpfr_cmp_si (op1, mpfr_get_d (op2, MPFR_RNDN)));
  printf("%d\n", mpfr_cmp_si_2exp (op1, mpfr_get_si (op2, MPFR_RNDN), mpfr_get_si (op2, MPFR_RNDN)));
  printf("%d\n", mpfr_cmpabs (op1, op2));
  printf("%d\n", mpfr_cmpabs_ui (op1, labs(mpfr_get_si (op2, MPFR_RNDN))));
  printf("%s\n", mpfr_nan_p (op1) != 0 ? "true" : "false");
  printf("%s\n", mpfr_inf_p (op1) != 0 ? "true" : "false");
  printf("%s\n", mpfr_number_p (op1) != 0 ? "true" : "false");
  printf("%s\n", mpfr_zero_p (op1) != 0 ? "true" : "false");
  printf("%s\n", mpfr_regular_p (op1) != 0 ? "true" : "false");
  printf("%s\n", mpfr_sgn (op1) >= 0 ? "Positive" : "Negative");
  printf("%s\n", mpfr_greater_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_greaterequal_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_less_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_lessequal_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_equal_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_lessgreater_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_unordered_p (op1, op2) != 0 ? "true" : "false");
  printf("%s\n", mpfr_total_order_p (op1, op2) != 0 ? "true" : "false");

  mpfr_clear (rop);
}

int main ()
{
  mpfr_t op1, op2;
  mpfr_inits (op1, op2, NULL);

  mpfr_set_d (op1, 1. / 3, MPFR_RNDN);
  mpfr_set_d (op2, 1. / 10, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_d (op1, 876.543 / 3.2, MPFR_RNDN);
  mpfr_set_d (op2, 123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_d (op1, -876.543 / 3.2, MPFR_RNDN);
  mpfr_set_d (op2, 123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_d (op1, -876.543 / 3.2, MPFR_RNDN);
  mpfr_set_d (op2, -123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_d (op1, 876.543 / 3.2, MPFR_RNDN);
  mpfr_set_d (op2, -123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_inf (op1, 1);
  mpfr_set_d (op2, -123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_set_nan (op1);
  mpfr_set_d (op2, -123.456 / 2.1, MPFR_RNDN);

  all(op1, op2);

  mpfr_clears (op1, op2, NULL);
  return 0;
}
