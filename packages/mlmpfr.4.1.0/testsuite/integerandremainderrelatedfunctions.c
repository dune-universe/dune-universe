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
  mpfr_t rop, sop;
  mpfr_init (rop);
  mpfr_init (sop);
  int ter;

  ter = mpfr_rint (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_ceil (rop, op1);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_floor (rop, op1);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_round (rop, op1);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_roundeven (rop, op1);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_trunc (rop, op1);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_rint_ceil (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_rint_floor (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_rint_round (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_rint_roundeven (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));
  ter = mpfr_rint_trunc (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_frac (rop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  ter = mpfr_modf (rop, sop, op1, MPFR_RNDN);
  mpfr_printf ("%.Re %s %.Re %s\n", rop, rounding_to_string (ter), sop, rounding_to_string (ter));

  ter = mpfr_fmod (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  long q = 0;
  ter = mpfr_fmodquo (rop, &q, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s %ld\n", rop, rounding_to_string (ter), q);

  ter = mpfr_remainder (rop, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s\n", rop, rounding_to_string (ter));

  q = 0;
  ter = mpfr_remquo (rop, &q, op1, op2, MPFR_RNDN);
  mpfr_printf ("%.Re %s %ld\n", rop, rounding_to_string (ter), q);

  ter = mpfr_integer_p (op1);
  printf ("%s\n", ter == 0 ? "false" : "true");

  mpfr_clear (rop);
  mpfr_clear (sop);
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
