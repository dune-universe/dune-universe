/*% $Id: matrix.h,v 1.3 2003/12/12 14:04:58 bjeannet Exp $ */

/* This header file exports operations on matrices. */

/*
A matrix is represented in the following manner: the coefficients are stored
in an \emph{private} array of \verb-pkint_t- \verb-p_init- of size
\verb-_maxrows*nbcolumns-. To access to elements, one use an array of
pointers \verb-p-, the $i^{\mbox{\scriptsize nth}}$ element of which points
to the $i^{\mbox{\scriptsize nth}}$ row of the matrix. This array is
initialized by the constructor. The advantage of this representation is to be
able to exchange easily rows of the matrix by exchanging the pointers,
without having to allocate at construction \verb-_maxrows- arrays for each
rows.  \verb-nbrows- indicates that only the first \verb-nbrows- rows are
used.
*/

#ifndef __POLKA_MATRIX_H__
#define __POLKA_MATRIX_H__

#include "config.h"
#include "polka.h"
#include "pkint.h"
#include "vector.h"
#include "satmat.h"

typedef struct matrix_t {
  /* public part */
  pkint_t** p;     /* array of pointers to rows */
  int nbrows;      /* number of effective rows */
  int nbcolumns;   /* size of rows */

  /* private part */
  pkint_t* _pinit; /* array of coefficients */
  int  _maxrows;   /* number of rows allocated */
  bool _sorted;     
} matrix_t;

/* Normal functions */

/* information about private part */
#define matrix_get_maxrows(mat) (mat->_maxrows)
#define matrix_is_sorted(mat) (mat->_sorted)
 /*c If defined as functions, the prototypes would be: */
 /*c int  matrix_get_maxrows(const matrix_t* const mat); */
 /*c bool matrix_is_sorted(const matrix_t* const mat); */

/* Basic Operations */
matrix_t* matrix_alloc(int nr, int nc, bool s);
void      matrix_free(matrix_t* mat);
void      matrix_clear(matrix_t* mat);
matrix_t* matrix_copy(const matrix_t* mat);
void      matrix_print(const matrix_t* mat);

/* Comparison \& Hashing */
int          matrix_compare(const matrix_t* ma,const matrix_t* mb);
unsigned int matrix_hash(const matrix_t* mat);
int          matrix_compare_sort(matrix_t* ma, matrix_t* mb);
unsigned int matrix_hash_sort(matrix_t* mat);

/* Operations on rows */
int  matrix_compare_rows(const matrix_t* mat, int l1, int l2);
void matrix_normalize_row(matrix_t* mat, int l);
void matrix_combine_rows(matrix_t* mat, int l1, int l2, int l3, int k);
void matrix_exch_rows(matrix_t* mat, int l1, int l2);

/* Sorting \& Merging */
void matrix_add_rows_sort(matrix_t* mat, matrix_t* cmat);
void matrix_sort_rows(matrix_t* mat);
void matrix_sort_rows_with_sat(matrix_t* mat, satmat_t* sat);
matrix_t* matrix_merge_sort(matrix_t* ma, matrix_t* mb);

/* Linear Transformation */
matrix_t* matrix_assign_variable(const matrix_t* mat, int var, const pkint_t* tab);
matrix_t* matrix_substitute_variable(const matrix_t* mat, int var, const pkint_t* tab);
matrix_t* matrix_assign_variables(const matrix_t* mat, const equation_t * eqn, int size);
matrix_t* matrix_substitute_variables(const matrix_t* mat, const equation_t * eqn, int size);


/* Predicates that can be useful for users */
#define matrix_is_row_dummy_constraint(mat,l) vector_is_dummy_constraint(mat->p[l],mat->nbcolumns)
 /*c If defined as functions, the prototypes would be: */
 /*c   bool matrix_is_row_dummy_constraint(const matrix* mat, int row); */

matrix_t* matrix_add_dimensions(const matrix_t* mat, int dimsup);
matrix_t* matrix_add_dimensions_multi(const matrix_t* mat, 
				      const dimsup_t* tab, int size);
matrix_t* matrix_remove_dimensions_multi(const matrix_t* mat, 
					 const dimsup_t* tab, int size);

matrix_t* matrix_add_permute_dimensions(const matrix_t* mat, int dimsup, const int * permutation);
matrix_t* matrix_permute_remove_dimensions(const matrix_t* mat, int dimsup, const int * permutation);

/* Functions meant to be internal */
matrix_t* _matrix_alloc_int(int nr, int nc, bool s);

#endif
