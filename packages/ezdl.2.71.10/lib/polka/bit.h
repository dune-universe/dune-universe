/*% $Id: bit.h,v 1.2 2003/12/12 14:02:17 bjeannet Exp $ */

/* This header file define operations on \emph{bitstrings} and
   \emph{bitindices}, to be used to access and modify bitstrings. */

/* The type \verb-bitstring_t- is simply an integer, which is an element
   of an array. 

   An structured index of a bit in a bitfield is a pair $(w,b)$ where $w$
   reference the considered integer and $b$ is a mask selecting the right
   bit.
*/

#ifndef __POLKA_BIT_H__
#define __POLKA_BIT_H__

#include "config.h"
#include "polka.h"

typedef unsigned short int bitstring_t;
typedef struct bitindex_t {
  int index;
  int word;
  bitstring_t bit;
} bitindex_t;

#define bitstring_size (sizeof(bitstring_t)*8)
#define bitstring_msb (1U<<(bitstring_size-1))

/* Operations on \verb-bitindex_t- */
void bitindex_print(const bitindex_t* bi);
bitindex_t bitindex_init(int col);
void bitindex_inc(bitindex_t*);
void bitindex_dec(bitindex_t*);
int bitindex_size(int n);

/* Operations on \verb-bitstring_t- */
bitstring_t* bitstring_alloc(int n);
void bitstring_free(bitstring_t* b);
void bitstring_clear(bitstring_t* b, int size);
int bitstring_cmp(const bitstring_t* r1, const bitstring_t* r2, int size);

void bitstring_print(const bitstring_t* b, int size);

int bitstring_get(const bitstring_t* b, bitindex_t ix);
void bitstring_set(bitstring_t* b, bitindex_t ix);
void bitstring_clr(bitstring_t* b, bitindex_t ix);

#endif
