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

void all (mpfr_t op1, mpfr_t op2, mpfr_exp_t exp)
{
  mpfr_t rop;
  mpfr_init (rop);

  mpfr_set (rop, op1, MPFR_RNDN);
  mpfr_nexttoward (rop, op2);
  mpfr_printf ("%.Re\n", rop);

  mpfr_set (rop, op1, MPFR_RNDN);
  mpfr_nextabove (rop);
  mpfr_printf ("%.Re\n", rop);
  mpfr_set (rop, op1, MPFR_RNDN);
  mpfr_nextbelow (rop);
  mpfr_printf ("%.Re\n", rop);

  mpfr_min (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);
  mpfr_max (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  printf ("%ld\n", mpfr_get_exp (op1));
  mpfr_set (rop, op1, MPFR_RNDN);
  mpfr_set_exp (rop, exp);
  printf ("%ld\n", mpfr_get_exp (rop));

  printf ("%s\n", mpfr_signbit (op1) == 0 ? "Positive" : "Negative");

  mpfr_copysign (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  printf ("%s\n", mpfr_get_version ());

  mpfr_clear (rop);
}

int main ()
{
  mpfr_t op1, op2;
  mpfr_inits (op1, op2, NULL);
  mpfr_exp_t p;

  p = 321;
  mpfr_set_d (op1, 1. / 3, MPFR_RNDN);
  mpfr_set_d (op2, 1. / 10, MPFR_RNDN);

  all(op1, op2, p);
  printf("\n");

  mpfr_set_d (op1, -4. / 3, MPFR_RNDN);
  p = 3;

  all(op1, op2, p);
  printf("\n");

  mpfr_set_d (op1, 1. / 3, MPFR_RNDN);
  mpfr_set_d (op2, -113. / 10, MPFR_RNDN);
  p = 89045;

  all(op1, op2, p);
  printf("\n");

  mpfr_set_d (op1, +0.1, MPFR_RNDN);
  mpfr_set_d (op2, -1. / 10, MPFR_RNDN);
  p = 808;

  all(op1, op2, p);
  printf("\n");

  mpfr_set_d (op1, -0.1, MPFR_RNDN);
  mpfr_set_d (op2, -0.1, MPFR_RNDN);
  p = 7;

  all(op1, op2, p);

  mpfr_clears (op1, op2, NULL);
  return 0;
}
