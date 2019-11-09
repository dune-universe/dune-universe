// gcc -L /usr/local/lib -o mpfr_version mpfr_version.c -lmpfr
// LD_LIBRARY_PATH=/usr/local/lib mpfr_version

#include <stdio.h>
#include <mpfr.h>

int main (void)
{
  printf ("%s\n", mpfr_get_version ());

  return 0;
}
