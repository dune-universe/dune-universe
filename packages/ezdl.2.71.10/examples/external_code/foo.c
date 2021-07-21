#include <stdlib.h>
#include <math.h>

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

// Uniformly draws an integer between O and max.
// Not the most useful function for Lutin...
EXPORT int rand_up_to(int min, int max){
  double r = ((double) random ());
  int res = min + ((int) ((r * (((double) (max-min+1)) / ((double) RAND_MAX)))));
  return res;
}
