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

void all (mpfr_t op, mpfr_rnd_t r1, mpfr_rnd_t r2, mpfr_exp_t err, mpfr_prec_t prec)
{
  mpfr_set_default_rounding_mode (MPFR_RNDN);
  printf ("%s\n", mpfr_print_rnd_mode (mpfr_get_default_rounding_mode ()));
  mpfr_set_default_rounding_mode (r1);
  printf ("%s\n", mpfr_print_rnd_mode (mpfr_get_default_rounding_mode ()));
  mpfr_prec_round (op, prec, r1);
  mpfr_printf ("%.Re\n", op);
  printf ("%s\n", mpfr_print_rnd_mode (mpfr_get_default_rounding_mode ()));
  printf ("%s\n", (mpfr_can_round (op, err, r1, r2, prec)) != 0 ? "true" : "false");
  printf ("%ld\n", mpfr_min_prec (op));
}

int main ()
{
  mpfr_t op;
  mpfr_prec_t prec;
  mpfr_rnd_t r1, r2;
  mpfr_exp_t err;
  mpfr_init (op);

  prec = 40;
  r1 = MPFR_RNDD;
  r2 = MPFR_RNDU;
  err = 1;
  mpfr_set_str (op, "1.101010111001001001100e17", 2, MPFR_RNDN);

  all (op, r1, r2, err, prec);

  prec = 80;
  r1 = MPFR_RNDD;
  r2 = MPFR_RNDZ;
  err = 400;
  mpfr_set_prec (op, prec);
  mpfr_set_d (op, 1. / 3., MPFR_RNDN);

  all (op, r1, r2, err, prec);

  prec = 800;
  r1 = MPFR_RNDA;
  r2 = MPFR_RNDZ;
  err = 400;
  mpfr_set_prec (op, prec);
  mpfr_set_d (op, 12345.78 / 3., MPFR_RNDN);

  all (op, r1, r2, err, prec);

  prec = 800;
  r1 = MPFR_RNDF;
  r2 = MPFR_RNDZ;
  err = 400;
  mpfr_set_prec (op, prec);
  mpfr_set_d (op, 12345.78 / 3., MPFR_RNDN);

  all (op, r1, r2, err, prec);

  prec = 80;
  r1 = MPFR_RNDA;
  r2 = MPFR_RNDZ;
  err = 40;
  mpfr_set_prec (op, prec);
  mpfr_set_nan (op);

  all (op, r1, r2, err, prec);

  prec = 80;
  r1 = MPFR_RNDA;
  r2 = MPFR_RNDZ;
  err = 40;
  mpfr_set_prec (op, prec);
  mpfr_set_inf (op, +1);

  all (op, r1, r2, err, prec);

  prec = 80;
  r1 = MPFR_RNDA;
  r2 = MPFR_RNDZ;
  err = 40;
  mpfr_set_prec (op, prec);
  mpfr_set_inf (op, -1);

  all (op, r1, r2, err, prec);

  mpfr_clear (op);
  return 0;
}
