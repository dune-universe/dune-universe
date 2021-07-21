/*% $Id: bit.c,v 1.2 2003/12/12 14:01:40 bjeannet Exp $ */

/*% Operations on bitstrings */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "bit.h"

/* %======================================================================== */
/* \section{Bitindices} */
/* %======================================================================== */

void bitindex_print(const bitindex_t* const bi)
{
  int k;
  bitstring_t m = bi->bit;
  assert (m!=0);
  k=(-1);
  do {
    k++;
    m >>= 1;
  } while (m!=0);
  printf("index=%d, word=%d, bit=%d\n",bi->index,bi->word,k);
}


/* \verb-bitindex_init()- takes as parameter a \emph{flat} index of a bit and
   returns the corresponding structured index.  \verb-bitindex_inc()- and
   \verb-bitindex_dec()- allow to increment and decrement an index.
   \verb-bitindex_size(n)- returns the size of an array of \verb-bitstring_t-
   containing \verb-n- bits. */

bitindex_t bitindex_init(const int col)
{
  bitindex_t res;
  res.index = col;
  res.word = col / bitstring_size;
  res.bit = bitstring_msb >> (col % bitstring_size);
  return res;
}

void bitindex_inc(bitindex_t* const bi){
  bi->index++;
  bi->bit >>= 1;
  if (bi->bit==0){
    bi->bit = bitstring_msb;
    bi->word++;
  }
}
void bitindex_dec(bitindex_t* const bi){
  bi->index--;
  if (bi->bit != bitstring_msb){
     bi->bit <<= 1;
  }
  else {
    bi->bit = 1;
    bi->word--;
  }
}

int bitindex_size(const int n){
  int size = n / bitstring_size;
  if (n % bitstring_size) size++;
  return size;
}

/* %======================================================================== */
/* \section{Bitstrings} */
/* %======================================================================== */

/*
  \verb-bitstring_alloc- allocates a new bitstring and
  \verb-bitstring_free()- frees the bitstring.

  \verb-bitstring_clear- sets to \verb-0- the bits, \verb-bitstring_cmp-
  compares two bitfields; be careful, it takes also in account unused bits of
  the last word. Last, \verb-bitstring_print()- writes the bits of a
  bitstring. 
*/
  
bitstring_t* bitstring_alloc(const int n){
  return (bitstring_t*)malloc(n*sizeof(bitstring_t));
}

void bitstring_free(bitstring_t* const b){
  free(b);
}

void bitstring_clear(bitstring_t* const b, const int size){
  int i;
  for (i=0; i<size; i++) b[i]=0;
}

void bitstring_print(const bitstring_t* const b, const int size)
{
  int j,k;
  bitstring_t m;

  for (j=0; j<size; j++){
    m = bitstring_msb; k = 1;
    while (m!=0) {
      if (b[j] & m) printf("1"); else printf("0");
      if (k % 8 == 0) printf(" ");
      else if (k % 4 == 0) printf(",");
      m >>= 1; k++;
    }
  }
}

int bitstring_cmp(const bitstring_t* const r1, const bitstring_t* const r2, const int size){
  int i;
  int res=0;
  for (i=0; i<size; i++){
    if (r1[i] < r2[i]){ res=-1; break; }
    else if (r1[i] > r2[i]){ res=1; break; }
  }
  return res;
}

/* These functions allow to read, set or clear individual bits of a bitstring, 
   referenced by a bitindex. */

int bitstring_get(const bitstring_t* const b, const bitindex_t ix) { 
  return b[ix.word] & ix.bit; 
}

void bitstring_set(bitstring_t* const b, const bitindex_t ix){
  b[ix.word] |= ix.bit; 
}

void bitstring_clr(bitstring_t* const b, const bitindex_t ix){ 
  b[ix.word] &= ~ix.bit; 
}


