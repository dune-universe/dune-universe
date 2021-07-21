/*% $Id: satmat.h,v 1.2 2004/02/27 10:22:29 bjeannet Exp $ */

/* This header file define operations on \emph{saturation matrices}. A
   saturation matrix is an array of bits (or an array of bitstrings) used to
   memorize wether a given generator saturate a given constraint. */

/* Saturation matrices are very similar to normal matrices as defined in
   \verb-matrix.h-, where explanations can be found. */

#ifndef __POLKA_SATMAT_H__
#define __POLKA_SATMAT_H__

#include "config.h"
#include "polka.h"
#include "bit.h"

typedef struct satmat_t {
  bitstring_t** p;
  int nbrows;
  int nbcolumns;
  bitstring_t* p_init;
} satmat_t;

satmat_t* satmat_alloc(int nr, int nc);
void satmat_free(satmat_t* sat);
void satmat_clear(satmat_t* sat);
satmat_t* satmat_copy(const satmat_t* sat);
void satmat_print(const satmat_t* sat);

bitstring_t satmat_get(const satmat_t* sat, int i, bitindex_t jx);
void satmat_set(satmat_t* sat, int i, bitindex_t jx);
void satmat_clr(satmat_t* sat, int i, bitindex_t jx);

satmat_t* satmat_transpose(const satmat_t* org, int nbcols);

void satmat_exch_rows(satmat_t* sat, int l1, int l2);
void satmat_sort_rows(satmat_t* sat);
int satmat_index_in_sorted_rows(const bitstring_t* satline, const satmat_t* sat);

#endif
