#include <stdint.h>

uint64_t x256pp_next(uint64_t *s);

/* This is the jump function for the generator. It is equivalent to 2^128 calls
   to next(); it can be used to generate 2^128 non-overlapping subsequences for
   parallel computations. */

void x256pp_jump(uint64_t *s);

/* This is the long-jump function for the generator. It is equivalent to 2^192
   calls to next(); it can be used to generate 2^64 starting points, from each
   of which jump() will generate 2^64 non-overlapping subsequences for parallel
   distributed computations. */

void x256pp_long_jump(uint64_t *s);
