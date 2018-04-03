#include <limits.h>
/* Test if x or y are negative, or if multiplying x * y would cause an
 * arithmetic overflow.
 */
#define oversized(x, y)						\
  ((x) < 0 || (y) < 0 || ((y) != 0 && (x) > INT_MAX / (y)))

#define failwith_oversized(lib) \
  failwith("#lib error: image contains oversized or bogus width and height");
