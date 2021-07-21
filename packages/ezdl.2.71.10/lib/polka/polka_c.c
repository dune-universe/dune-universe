/*% $Id: polka.c,v 1.5 2004/02/27 10:34:10 bjeannet Exp $ */

/* \section{Declarations} */

/* Types, operations and variables that are global to the library. */

#include "config.h"
#include "polka.h"
#include "internal.h"
#include "vector.h"
#include "matrix.h"



const int polka_integer = POLKA_NUM;

/* \section{Lattice tbool} */

tbool tbool_union(const tbool a, const tbool b)
{
  if (a==tbool_top || b==tbool_bottom) return a;
  else if (b==tbool_top || a==tbool_bottom) return b;
  else if (a!=b) return tbool_top; else return a;
}
tbool tbool_inter(const tbool a, const tbool b)
{
  if (a==tbool_top || b==tbool_bottom) return b;
  else if (b==tbool_top || a==tbool_bottom) return a;
  else if (a!=b) return tbool_bottom; else return a;
}

/* \section{Global variables and functions} */

bool polka_strict = false;
int polka_dec = 2;
bool polka_widening_affine = false;

int polka_maxnbdims = 0;
int polka_maxnbrows = 0;
int polka_maxcolumns = 0;

void polka_initialize(bool strict, int maxnbdims, int maxnbrows)
{
  polka_strict = strict;
  polka_dec = strict ? 3 : 2;
  polka_maxnbdims = maxnbdims;
  polka_maxnbrows = maxnbrows;
  polka_maxcolumns = polka_dec + maxnbdims;

  pk_vector_pkintp = vector_alloc(polka_maxcolumns);
  pk_vector_tmp = vector_alloc(5);

  pk_matrix_pkintpp = (pkint_t**)malloc(polka_maxnbrows*sizeof(pkint_t*));
  pkint_init(pk_matrix_acc);
  pkint_init(pk_matrix_prod);

  pk_cherni_bigray = matrix_alloc(polka_maxnbrows,polka_maxcolumns,false);
  pk_cherni_bigsatc = satmat_alloc(polka_maxnbrows, bitindex_size(polka_maxnbrows));
  pk_cherni_bitstringp = bitstring_alloc(bitindex_size(polka_maxnbrows));
  pk_cherni_intp = (int*)malloc(polka_maxcolumns * sizeof(int));
  pkint_init(pk_cherni_prod);

  pk_poly_pkintp = vector_alloc(polka_maxcolumns);
  pk_poly_bitstringp = bitstring_alloc(bitindex_size(polka_maxnbrows));
  pk_poly_matspecial = matrix_alloc(1,polka_maxcolumns,true);
  vector_free(pk_poly_matspecial->_pinit,polka_maxcolumns); pk_poly_matspecial->_pinit = 0;
  pkint_init(pk_poly_prod);
}

void polka_finalize(void)
{
  if (pk_vector_pkintp) vector_free(pk_vector_pkintp,polka_maxcolumns);
  if (pk_vector_tmp) vector_free(pk_vector_tmp,5);
  pk_vector_tmp = 0;
  pk_vector_pkintp = 0;

  if (pk_matrix_pkintpp) free(pk_matrix_pkintpp);
  pk_matrix_pkintpp = 0;
  pkint_clear(pk_matrix_acc);
  pkint_clear(pk_matrix_prod);

  if (pk_cherni_bigray) matrix_free(pk_cherni_bigray);
  if (pk_cherni_bigsatc) satmat_free(pk_cherni_bigsatc);
  if (pk_cherni_bitstringp) free(pk_cherni_bitstringp);
  if (pk_cherni_intp) free(pk_cherni_intp);
  pk_cherni_bigray = 0;
  pk_cherni_bigsatc = 0;
  pk_cherni_bitstringp = 0;
  pk_cherni_intp = 0;
  pkint_clear(pk_cherni_prod);

  if (pk_poly_pkintp) vector_free(pk_poly_pkintp, polka_maxcolumns);
  if (pk_poly_bitstringp) bitstring_free(pk_poly_bitstringp);
  if (pk_poly_matspecial) matrix_free(pk_poly_matspecial);
  pk_poly_pkintp = 0;
  pk_poly_bitstringp = 0;
  pk_poly_matspecial = 0;
  pkint_clear(pk_poly_prod);

  polka_maxnbdims=0;
  polka_maxnbrows=0;
  polka_maxcolumns=0;
}

void polka_set_widening_affine()
{
  polka_widening_affine = true;
}
void polka_set_widening_linear()
{
  polka_widening_affine = false;
}

/* \section{Operations on dimsup\_t[]} */

int dimsup_of_multi(const dimsup_t* tab, int size)
{
  int m,dimsup;

  dimsup = 0;
  for (m=0; m<size; m++)
    dimsup += tab[m].nbdims;
  return dimsup;
}

int check_add_dimsup_of_multi(int nbcols, const dimsup_t* tab, int size)
{
  int m,dimsup;

  dimsup = 0;
  for (m=0; m<size; m++){
    if (tab[m].pos < 0 || tab[m].pos > nbcols - polka_dec + 1){
      fprintf(stderr,
	      "check_add_dimsup_of_multi: inconsistent tab[%d].pos=%d\n",
	      m,tab[m].pos);
      abort();
    }
    if (tab[m].nbdims<=0){
      fprintf(stderr,
	      "check_add_dimsup_of_multi : inconsistent tab[%d].nbdims = %d\n",
	      m,tab[m].nbdims);
      abort();
    }
    dimsup += tab[m].nbdims;
  }
  if (nbcols+dimsup > polka_maxcolumns){
    fprintf(stderr,
	    "check_add_dimsup_of_multi : nbcols+dimsup = %d+%d exceeding polka_maxcolumns=%d\n",
	    nbcols,dimsup,polka_maxcolumns);
    abort();
  }
  return dimsup;
}

int check_remove_dimsup_of_multi(int nbcols, const dimsup_t* tab, int size)
{
  int m,dimsup;

  dimsup = 0;
  for (m=0; m<size; m++){
    if (tab[m].pos < dimsup || tab[m].pos > dimsup + nbcols - polka_dec + 1){
      fprintf(stderr,
	      "check_remove_dimsup_of_multi: inconsistent tab[%d].pos=%d\n",
	      m,tab[m].pos);
      abort();
    }
    if (tab[m].nbdims<=0){
      fprintf(stderr,
	      "check_remove_dimsup_of_multi : inconsistent tab[%d].nbdims = %d\n",
	      m,tab[m].nbdims);
      abort();
    }
    dimsup += tab[m].nbdims;
  }
  if (nbcols-dimsup < polka_dec){
    fprintf(stderr,
	    "check_remove_dimsup_of_multi : nbcols-dimsup = %d+%d < polka_dec=%d\n",
	    nbcols,dimsup,polka_dec);
    abort();
  }
  return dimsup;
}

/* \section{Operations on equation\_t[]} */

/* Add \verb|offset| to the field \verb|var| of the equations
   belonging to the array \verb|eqn| of size \verb|size|. Also,
   perform some check using the variable \verb|nbcols| indicating the
   size of matrices.
*/

void translate_equations(equation_t * eqn, const int size, const int offset)
{
  int i;
  for (i=0; i<size; i++){
    eqn[i].var += offset;
  }
}

/* Compare the field \verb|var| of two equations  */

int cmp_equations(const equation_t* eqna, const equation_t* eqnb)
{
  return (eqna->var - eqnb->var);
}

/* Sort the array \verb|eqn| of size \verb|size| according to the field
   \verb|var| in ascending order.
*/

void sort_equations(equation_t * eqn, const int size)
{
  qsort(eqn,(size_t)size,sizeof(equation_t),(int(*)(const void*,const void*))cmp_equations);
}
