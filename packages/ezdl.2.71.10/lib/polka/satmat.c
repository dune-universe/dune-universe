/*% $Id: satmat.c,v 1.2 2004/02/27 10:22:28 bjeannet Exp $ */

/*% Operations on saturation matrices */

#include <stdlib.h>
#include <stdio.h>
#include "config.h"
#include "satmat.h"

/* %======================================================================== */
/* \section[Basic operations]
   {Basic operations: creation, destruction, copying and printing} */
/* %======================================================================== */

/* Set all bits to zero. */
void satmat_clear(satmat_t* const sat)
{
  int i,j;
  for (i=0; i<sat->nbrows; i++)
    for (j=0; j<sat->nbcolumns; j++)
      sat->p[i][j] = 0;
}

/* Standard allocation function, with initialization of the elements. */
satmat_t* satmat_alloc(const int nr, const int nc)
{
  satmat_t* const sat = (satmat_t*)malloc(sizeof(satmat_t));
  sat->nbrows = nr;
  sat->nbcolumns = nc;
  if (nr*nc>0){
    int i;
    bitstring_t* q;
    sat->p_init = bitstring_alloc(nr*nc);
    sat->p = (bitstring_t**)malloc(nr*sizeof(bitstring_t*));
    q = sat->p_init;
    for (i=0; i<nr; i++){
      sat->p[i]=q;
      q=q+nc;
    }
    satmat_clear(sat);
  }
  else {
    sat->p_init = 0;
    sat->p = 0;
  }
  return sat;
}

/* Deallocation function. */
void satmat_free(satmat_t* const sat)
{
  if (sat->p) free(sat->p);
  if(sat->p_init) free(sat->p_init);
  free(sat);
}

/* Create a copy of the used part. */
satmat_t* satmat_copy(const satmat_t* const sat)
{
  int i,j;
  satmat_t* const nsat = satmat_alloc(sat->nbrows,sat->nbcolumns);
  for (i=0; i<sat->nbrows; i++){
    for (j=0; j<sat->nbcolumns; j++){
      nsat->p[i][j] = sat->p[i][j];
    }
  }
  return nsat;
}

/* Raw printing function. */
void satmat_print(const satmat_t* const sat)
{
  int i;

  printf("%d %d\n",sat->nbrows,sat->nbcolumns);
  for (i=0; i<sat->nbrows; i++){
    bitstring_print(sat->p[i],sat->nbcolumns);
    printf("\n");
  }
}

/* %======================================================================== */
/* \section{Bit operations} */
/* %======================================================================== */

/* These function allow to read and to clear or set individual bits. \verb-i-
   indicates the row and \verb-jx- the column. */

bitstring_t satmat_get(const satmat_t* const sat, const int i, const bitindex_t jx){
  return bitstring_get(sat->p[i],jx);
}
void satmat_set(satmat_t* const sat, const int i, const bitindex_t jx){
  bitstring_set(sat->p[i],jx);
}
void satmat_clr(satmat_t* const sat, const int i, const bitindex_t jx){
  bitstring_clr(sat->p[i],jx);
}

/* %======================================================================== */
/* \section{Matrix operations} */
/* %======================================================================== */

/* Transposition.

\verb-nbcols- indicates the number of bits to be transposed (the number of columns of the matrix is the size of the row of \verb-bitstring_t-, not the number of bits really used). */
satmat_t* satmat_transpose(const satmat_t* const org, const int nbcols)
{
  bitindex_t i,j;
  satmat_t* const dest = satmat_alloc(nbcols,bitindex_size(org->nbrows));

  for (i = bitindex_init(0); i.index < org->nbrows; bitindex_inc(&i) ) {
    for (j = bitindex_init(0); j.index < nbcols; bitindex_inc(&j) ){
      if (satmat_get(org,i.index,j)) satmat_set(dest,j.index,i);
    }
  }
  return dest;
}

/* Row exchange. */
void satmat_exch_rows(satmat_t* const sat, const int l1, const int l2)
{
  bitstring_t* const aux=sat->p[l1];
  sat->p[l1]=sat->p[l2];
  sat->p[l2]=aux;
}

/* Row sorting.

We use here the insertion sort. */
static void satmat_isort_rows(satmat_t* const sat)
{
  int i,j;

  for (i=1; i<sat->nbrows; i++){
    bitstring_t* const row = sat->p[i];
    j = i;
    while (j > 0 && bitstring_cmp(sat->p[j-1],row,sat->nbcolumns) > 0){
      sat->p[j] = sat->p[j-1];
      j--;
    }
    sat->p[j] = row;
  }
}

/* Row sorting.

We use here the quick sort. */
static int qsort_size = 0;
static int qsort_rows_compar(const void* p1, const void* p2)
{
  return (bitstring_cmp( *(bitstring_t**)p1,
			 *(bitstring_t**)p2,
			 qsort_size));
}

void satmat_sort_rows(satmat_t* const sat)
{
  if (sat->nbrows>=6){
    qsort_size = sat->nbcolumns;
    qsort((void*)sat->p,
	  (size_t)sat->nbrows, sizeof(bitstring_t*),
	  qsort_rows_compar);
  }
  else {
    satmat_isort_rows(sat);
  }
}

/* Membership test.

The following function tests if the given row belongs to the sorted saturation
matrix. If it is the case, it returns its rank in the saturation
matrix. Otherwise, it returns -1 */

static const bitstring_t* index_satline = NULL;
static bitstring_t** index_p = NULL;
static int index_size = 0;

static bool index2(int low, int high)
{
  if (high - low <= 4){
    int i;
    int res=-1;
    for (i=low; i<high; i++){
      int cmp = bitstring_cmp(index_p[i],index_satline,index_size);
      if (cmp==0){
	res=i; break;
      }
      else if (cmp>0) break;
    }
    return res;
  }
  else {
    int mid = low+(high-low)/2;
    int cmp = bitstring_cmp(index_p[mid],index_satline,index_size);
    if (cmp<0)
      return (index2(mid+1,high));
    else if (cmp>0)
      return (index2(low,mid));
    else
      return mid;
  }
}

int
satmat_index_in_sorted_rows(const bitstring_t* const satline, const satmat_t* const sat)
{
  index_satline = satline;
  index_p = sat->p;
  index_size = sat->nbcolumns;
  return (index2(0,sat->nbrows));
}

/*% /\* Column exchange. *\/ */

/*% /\* The following function exchanges the bits of the columns \verb-i- and */
/*%    \verb-j- for rows of the matrix starting at \verb-nbline-. *\/ */

/*% void  */
/*% satmat_exch_cols(satmat_t* const sat, const int nbline, const bitindex_t ix, const bitindex_t jx) */
/*% { */
/*%   int k; */
/*%   bitstring_t tmp; */

/*%   if (ix.word == jx.word){ */
/*%     register bitstring_t word; */
/*%     for (k=nbline; k<sat->nbrows; k++){ */
/*%       word = sat->p[k][ix.word]; */
/*%       tmp = word & ix.bit; */
/*%       word = (word & jx.bit) ? word | ix.bit : word & (~ix.bit); */
/*%       sat->p[k][ix.word] = tmp ? word | jx.bit : word & (~jx.bit); */
/*%     } */
/*%   } */
/*%   else { */
/*%     for (k=nbline; k<sat->nbrows; k++){ */
/*%       register bitstring_t word = sat->p[k][ix.word]; */
/*%       tmp = word & ix.bit; */
/*%       sat->p[k][ix.word] =  */
/*%	(sat->p[k][jx.word] & jx.bit) ? word | ix.bit : word & (~ix.bit); */
/*%       sat->p[k][jx.word] =  */
/*%	tmp ? sat->p[k][jx.word] | jx.bit : sat->p[k][jx.word] & (~jx.bit); */
/*%     } */
/*%   } */
/*% } */

/*% /\* Columns affectation. *\/ */

/*% /\* The following function copies the bits of the column \verb-j- in the */
/*%    column \verb-i- for rows of the matrix starting at \verb-nbline-. *\/ */

/*% void satmat_assign_col(satmat_t* const sat, const int nbline, const bitindex_t ix, const bitindex_t jx) */
/*% { */
/*%   int k; */

/*%   if (ix.word == jx.word){ */
/*%     register bitstring_t word; */
/*%     for (k=nbline; k<sat->nbrows; k++){ */
/*%       word = sat->p[k][ix.word]; */
/*%       sat->p[k][ix.word] = (word & jx.bit) ? word | ix.bit : word & ~ix.bit; */
/*%     } */
/*%   } */
/*%   else { */
/*%     for (k=nbline; k<sat->nbrows; k++) */
/*%       if (sat->p[k][jx.word] & jx.bit)  */
/*%	sat->p[k][ix.word] |= ix.bit;  */
/*%       else  */
/*%	sat->p[k][ix.word] &= ~ix.bit; */
/*%   } */
/*% } */
