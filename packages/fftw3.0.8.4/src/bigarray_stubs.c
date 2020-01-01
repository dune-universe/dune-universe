/* Verbatim copy of functions in "bigarray_stubs.c" of the OCaml
   distribution that are needed for the specially allocated fftw
   arrays (not defined here).  Everything must be declared
   "static". */

#include <stdint.h>

#if OCAML_VERSION < 41000
/* From ocaml/byterun/compare.h */
CAMLextern int caml_compare_unordered;
#endif

/* From bigarray.h in latest versions of OCaml */
#ifndef SIZEOF_BA_ARRAY
#if (__STDC_VERSION__ >= 199901L)
#define SIZEOF_BA_ARRAY sizeof(struct caml_ba_array)
#else
#define SIZEOF_BA_ARRAY (sizeof(struct caml_ba_array) - sizeof(intnat))
#endif
#endif /* SIZEOF_BA_ARRAY */

#if OCAML_VERSION < 40600
#include "bigarray_stubs_old.c"
#endif


static uintnat
caml_ba_multov(uintnat a, uintnat b, int * overflow)
{
#define HALF_SIZE (sizeof(uintnat) * 4)
#define HALF_MASK (((uintnat)1 << HALF_SIZE) - 1)
#define LOW_HALF(x) ((x) & HALF_MASK)
#define HIGH_HALF(x) ((x) >> HALF_SIZE)
  /* Cut in half words */
  uintnat al = LOW_HALF(a);
  uintnat ah = HIGH_HALF(a);
  uintnat bl = LOW_HALF(b);
  uintnat bh = HIGH_HALF(b);
  /* Exact product is:
              al * bl
           +  ah * bl  << HALF_SIZE
           +  al * bh  << HALF_SIZE
           +  ah * bh  << 2*HALF_SIZE
     Overflow occurs if:
        ah * bh is not 0, i.e. ah != 0 and bh != 0
     OR ah * bl has high half != 0
     OR ah * bl has high half != 0
     OR the sum al * bl + LOW_HALF(ah * bl) << HALF_SIZE
                        + LOW_HALF(al * bh) << HALF_SIZE overflows.
     This sum is equal to p = (a * b) modulo word size. */
  uintnat p1 = al * bh;
  uintnat p2 = ah * bl;
  uintnat p = a * b;
  if (ah != 0 && bh != 0) *overflow = 1;
  if (HIGH_HALF(p1) != 0 || HIGH_HALF(p2) != 0) *overflow = 1;
  p1 <<= HALF_SIZE;
  p2 <<= HALF_SIZE;
  p1 += p2;
  if (p < p1 || p1 < p2) *overflow = 1; /* overflow in sums */
  return p;
#undef HALF_SIZE
#undef LOW_HALF
#undef HIGH_HALF
}


#if OCAML_VERSION < 40800
#define CAML_BA_MAX_MEMORY 1024*1024*1024
#endif

static void caml_ba_deserialize_longarray(void * dest, intnat num_elts)
{
  int sixty = caml_deserialize_uint_1();
#ifdef ARCH_SIXTYFOUR
  if (sixty) {
    caml_deserialize_block_8(dest, num_elts);
  } else {
    intnat * p, n;
    for (n = 0, p = dest; n < num_elts; n++, p++)
      *p = caml_deserialize_sint_4();
  }
#else
  if (sixty)
    caml_deserialize_error("input_value: cannot read bigarray "
                           "with 64-bit OCaml ints");
  caml_deserialize_block_4(dest, num_elts);
#endif
}
