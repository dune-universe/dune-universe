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
#include <stdio.h>
#include <mpfr.h>

int main(int argc, char **argv)
{
  mpfr_t x, y, z;
  double d;
  int i;
  mpfr_exp_t e;

  if (argc != 2)
    exit (1);

  mpfr_init (x);
  mpfr_set_d (x, 1./3., MPFR_RNDN);

  d = mpfr_get_d (x, MPFR_RNDN);
  printf ("%e\n", d);

  i = mpfr_get_si (x, MPFR_RNDN);
  printf ("%d\n", i);

  d = mpfr_get_d_2exp (&e, x, MPFR_RNDN);
  printf("%e %ld\n", d, e);

  mpfr_init (y);
  mpfr_frexp (&e, y, x, MPFR_RNDN);
  printf("%ld\n", e);

  char *buffer;
  buffer = mpfr_get_str (NULL, &e, 10, 0, x, MPFR_RNDN);
  printf("%s %ld\n", buffer, e);

  mpfr_printf ("%.Re\n", x);

  FILE *stream;
  stream = fopen ("fpif_export.expected", "w");
  mpfr_fpif_export (stream, x);
  fclose (stream);

  mpfr_init (z);
  stream = fopen (argv[1], "r");
  mpfr_fpif_import (z, stream);
  mpfr_printf ("%.Re\n", z);
  fclose (stream);

  if (mpfr_fits_sint_p (x, MPFR_RNDN))
    printf ("true\n");
  else
    printf ("false\n");

  mpfr_free_str (buffer);
  mpfr_clear (x);
  mpfr_clear (y);
  return 0;
}
