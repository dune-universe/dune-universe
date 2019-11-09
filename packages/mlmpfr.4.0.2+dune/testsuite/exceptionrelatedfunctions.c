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

void flags ()
{
  printf ("%s %s %s %s %s %s\n",
	  mpfr_underflow_p () ? "true" : "false",
	  mpfr_overflow_p () ? "true" : "false",
	  mpfr_divby0_p () ? "true" : "false",
	  mpfr_nanflag_p () ? "true" : "false",
	  mpfr_inexflag_p () ? "true" : "false",
	  mpfr_erangeflag_p () ? "true" : "false");
}

void all (mpfr_t op, int t, mpfr_exp_t em1, mpfr_exp_t em2)
{
  mpfr_t rop;
  mpfr_init2 (rop, mpfr_get_prec (op));

  printf ("%ld %ld\n", mpfr_get_emin (), mpfr_get_emax ());
  mpfr_set_emin (em1);
  mpfr_set_emax (em2);
  printf ("%ld %ld\n", mpfr_get_emin (), mpfr_get_emax ());

  printf ("%ld %ld %ld %ld\n", mpfr_get_emin_min (), mpfr_get_emin_max (), mpfr_get_emax_min (), mpfr_get_emax_max ());

  mpfr_set (rop, op, MPFR_RNDN);
  mpfr_check_range (rop, t, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  mpfr_set (rop, op, MPFR_RNDN);
  mpfr_subnormalize (rop, t, MPFR_RNDN);
  mpfr_printf ("%.Re\n", rop);

  flags ();

  mpfr_set_underflow ();
  mpfr_set_overflow ();
  mpfr_set_divby0 ();
  mpfr_set_nanflag ();
  mpfr_set_inexflag ();
  mpfr_set_erangeflag ();

  flags ();

  mpfr_clear_underflow ();
  mpfr_clear_overflow ();
  mpfr_clear_divby0 ();
  mpfr_clear_nanflag ();
  mpfr_clear_inexflag ();
  mpfr_clear_erangeflag ();

  flags ();

  mpfr_flags_set(MPFR_FLAGS_NAN|MPFR_FLAGS_ERANGE|MPFR_FLAGS_DIVBY0);
  flags ();
  int saved_flags = mpfr_flags_save();
  int test_all_flags = mpfr_flags_test(MPFR_FLAGS_ALL);
  int test_one_flag = mpfr_flags_test(MPFR_FLAGS_ERANGE);
  printf ("%d %d\n", test_all_flags, test_one_flag);
  mpfr_flags_clear(MPFR_FLAGS_ALL);
  flags ();
  mpfr_flags_restore(saved_flags, MPFR_FLAGS_ERANGE|MPFR_FLAGS_DIVBY0|MPFR_FLAGS_OVERFLOW);
  flags ();

  mpfr_clear (rop);
}

int main ()
{
  mpfr_t op;
  mpfr_init (op);
  mpfr_exp_t e1, e2;
  int t;

  e1 = -123;
  e2 = 123;
  t = mpfr_set_d (op, 1. / 3, MPFR_RNDN);

  all(op, t, e1, e2);
  printf("\n");

  mpfr_clear (op);
  return 0;
}
