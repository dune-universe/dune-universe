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

int main()
{
  mpfr_set_default_prec (12345);
  printf ("default_precision: %ld\n", mpfr_get_default_prec ());

  char* buffer;
  mpfr_t one, x;
  mpfr_inits (one, x, NULL);

  int r = mpfr_set_si (one, 1, MPFR_RNDN);
  buffer = rounding_to_string (r);
  printf ("%s\n", buffer);
  free (buffer);

  r = mpfr_set (x, one, MPFR_RNDN);
  buffer = rounding_to_string (r);
  printf ("%s\n", buffer);
  free (buffer);

  r = mpfr_set_d (x, 1., MPFR_RNDN);
  buffer = rounding_to_string (r);
  printf ("%s\n", buffer);
  free (buffer);

  r = mpfr_set_str (x, "1", 0, MPFR_RNDN);
  buffer = rounding_to_string (r);
  printf ("%s\n", buffer);
  free (buffer);

  mpfr_set_nan (x);
  printf ("None\n");

  mpfr_set_inf (x, 1);
  printf ("None\n");

  mpfr_set_zero (x, -1);
  printf ("None\n");

  printf ("%ld\n", mpfr_get_prec (x));


  mpfr_clears (one, x, NULL);
  return 0;
}
