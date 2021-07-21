/*% $Id: matrix.c,v 1.3 2003/12/12 14:04:58 bjeannet Exp $ */

/*% Operations on matrices */

#include <stdio.h>
#include <assert.h>
#include "polka.h"
#include "matrix.h"
#include "internal.h"

/* %======================================================================== */
/* \section[Basic operations]
   {Basic operations: creation, destruction, copying and printing} */
/* %======================================================================== */

/* Internal allocation function: the elements are not initialized.
   \verb-mr- is the maximum number of rows, and \verb-nc- the number of
   columns. By default, \verb-nbrows- is initialized to \verb-mr- . */
matrix_t* _matrix_alloc_int(const int mr, const int nc, const bool s)
{
  matrix_t* mat = (matrix_t*)malloc(sizeof(matrix_t));
  mat->nbrows = mat->_maxrows = mr;
  mat->nbcolumns = nc;
  mat->_sorted = s;
  if (mr*nc>0){
    int i;
    pkint_t* q;
    mat->_pinit = _vector_alloc_int(mr*nc);
    mat->p = (pkint_t**)malloc(mr * sizeof(pkint_t*));
    q = mat->_pinit;
    for (i=0;i<mr;i++){
      mat->p[i]=q;
      q=q+nc;
    }
  }
  else {
    mat->_pinit = 0;
    mat->p = 0;
  }
  return mat;
}

/* Standard allocation function, with initialization of the elements. */
matrix_t* matrix_alloc(const int mr, const int nc, const bool s)
{
  matrix_t* mat = (matrix_t*)malloc(sizeof(matrix_t));
  mat->nbrows = mat->_maxrows = mr;
  mat->nbcolumns = nc;
  mat->_sorted = s;
  if (mr*nc>0){
    int i;
    pkint_t* q;
    mat->_pinit = vector_alloc(mr*nc);
    mat->p = (pkint_t**)malloc(mr * sizeof(pkint_t*));
    q = mat->_pinit;
    for (i=0;i<mr;i++){
      mat->p[i]=q;
      q=q+nc;
    }
  }
  else {
    mat->_pinit = 0;
    mat->p = 0;
  }
  return mat;
}

/* Deallocation function. */
void matrix_free(matrix_t* const mat)
{
  if (mat->p) free(mat->p);
  if (mat->_pinit) vector_free(mat->_pinit, mat->_maxrows * mat->nbcolumns);
  free(mat);
}

/* Set all elements to zero. */
void matrix_clear(matrix_t* const mat)
{
  if (mat->_pinit){
    int i,j;
    for (i=0; i<mat->nbrows; i++){
      for (j=0; j<mat->nbcolumns; j++){
	pkint_set_si(mat->p[i][j],0);
      }
    }
  }
}

/* Create a copy of the matrix of size \verb-nbrows- (and not
   \verb-_maxrows-). Only ``used'' rows are copied. */
matrix_t* matrix_copy(const matrix_t* const mat)
{
  int i,j;
  matrix_t* nmat = _matrix_alloc_int(mat->nbrows,mat->nbcolumns,mat->_sorted);
  if (mat->_pinit){
    for (i=0;i<mat->nbrows;i++){
      for (j=0; j<mat->nbcolumns; j++){
	pkint_init_set(nmat->p[i][j],mat->p[i][j]);
      }
    }
  }
  return nmat;
}

/* Raw printing function. */
void matrix_print(const matrix_t* const mat)
{
  int i,j;
  printf("%d %d\n", mat->nbrows, mat->nbcolumns);
  if (mat->_pinit){
    for (i=0;i<mat->nbrows;i++) {
      for (j=0;j<mat->nbcolumns;j++){
	pkint_print(mat->p[i][j]);
	printf(" ");
    }
      printf("\n");
    }
  }
}


/* %======================================================================== */
/* \section{Comparison and hashing functions} */
/* %======================================================================== */

/* Comparison function.

Two matrices are equal if their \emph{used} dimensions are the same and if
their rows matches each other. The order is compatible with that defined on
vectors, that is, if for the first non equal row \verb-i-, \verb-ma->p[i-] is
less than \verb-mb->p[i-], then the result is negative. */

int matrix_compare(const matrix_t* const ma,const matrix_t* const mb)
{
  int i;
  int res;
  res = ma->nbrows - mb->nbrows;
  if (res==0){
    res = ma->nbcolumns - mb->nbcolumns;
    if (res==0){
      if (ma->_pinit && mb->_pinit){
	for (i=0; i<ma->nbrows; i++){
	  res = vector_compare(ma->p[i],mb->p[i],ma->nbcolumns);
	  if (res) break;
	}
      }
    }
  }
  return res;
}

/* Hashing function.

It is compatible with the above notion of equality. */
unsigned int matrix_hash(const matrix_t* const mat)
{
  if (mat->_pinit){
    int i;
    unsigned int res = 0;
    unsigned int dec = 0;
    for (i=0; i<mat->nbrows; i += (mat->nbrows+3)/4){
      res += vector_hash(mat->p[i],mat->nbcolumns) << dec;
      dec++;
    }
    return res;
  }
  else {
    return 0;
  }
}

/* The next two functions are identical as the preceding ones, but consider
   matrices as sets of vectors: order of rows does not matter. */

/* Comparison function. */
int matrix_compare_sort(matrix_t* const ma, matrix_t* const mb)
{
  int res;
  res = ma->nbrows - mb->nbrows;
  if (res==0){
    res = ma->nbcolumns - mb->nbcolumns;
    if (res==0){
      if (ma->_pinit && mb->_pinit){
	if (!ma->_sorted) matrix_sort_rows(ma);
	if (!mb->_sorted) matrix_sort_rows(mb);
	res = matrix_compare(ma,mb);
      }
    }
  }
  return res;
}

/* Hashing function.

It is compatible with the above notion of equality. */
unsigned int matrix_hash_sort(matrix_t* const mat)
{
  int res;
  if (mat->_pinit){
    if (!mat->_sorted) matrix_sort_rows(mat);
    res = matrix_hash(mat);
    return res;
  }
  else {
    return 0;
  }
}

/* %======================================================================== */
/* \section{Operation on rows} */
/* %======================================================================== */

/* \verb-compare_rows- compares rows of matrix, \verb-exch_rows- exchanges
   two rows; \verb-normalize_row- normalizes a row of a matrix but without
   considering the first coefficient; \verb-combine_rows- combine rows
   \verb-l1- and \verb-l2- and puts the result in \verb-l3- such that
   \verb-l3[k-] is zero. */

int matrix_compare_rows(const matrix_t* const mat, const int l1, const int l2){
  return vector_compare(mat->p[l1],mat->p[l2],mat->nbcolumns);
}
void matrix_normalize_row(matrix_t* const mat, const int l){
  vector_normalize(mat->p[l],mat->nbcolumns);
}
void matrix_combine_rows(matrix_t* const mat, const int l1, const int l2, const int l3, const int k){
  vector_combine(mat->p[l1],mat->p[l2],mat->p[l3],k,mat->nbcolumns);
}
void matrix_exch_rows(matrix_t* const mat, const int l1, const int l2)
{
  pkint_t* aux=mat->p[l1];
  mat->p[l1]=mat->p[l2];
  mat->p[l2]=aux;
}

/* %======================================================================== */
/* \section{Sorting \& Merging} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Sorting} */
/* %------------------------------------------------------------------------ */

/* We use here the insertion sort, modified to remove doublons. */

static void matrix_isort_rows(matrix_t* const mat)
{
  if (!mat->_sorted){
    int i = 1;
    while(i<mat->nbrows) {
      pkint_t* row = mat->p[i];
      int j = i;
      int s = 1;
      while (j > 0){
	s = vector_compare(mat->p[j-1],row,mat->nbcolumns);
	if (s<=0) break;
	mat->p[j] = mat->p[j-1];
	j--;
      }
      if (s==0){ /* doublons ! */
	/* on retablit la situation */
	while (j<i){
	  mat->p[j] = mat->p[j+1]; j++;
	}
	mat->p[i] = row;
	/* on met le doublon a la fin */
	mat->nbrows--;
	matrix_exch_rows(mat,i,mat->nbrows);
      } else {
	mat->p[j] = row;
	i++;
      }
    }
    mat->_sorted = true;
  }
}

/* This variant permutes also the saturation matrix together with the matrix.
   There is here no handling of doublons. */

static void matrix_isort_rows_with_sat(matrix_t* const mat, satmat_t* const sat)
{
  if (!mat->_sorted){
    int i = 1;
    while(i<mat->nbrows) {
      pkint_t* row = mat->p[i];
      bitstring_t* rowsat = sat->p[i];
      int j = i;
      int s = 1;
      while (j > 0){
	s = vector_compare(mat->p[j-1],row,mat->nbcolumns);
	assert(s!=0);
	if (s<=0) break;
	mat->p[j] = mat->p[j-1];
	sat->p[j] = sat->p[j-1];
	j--;
      }
      mat->p[j] = row;
      sat->p[j] = rowsat;
      i++;
    }
    mat->_sorted = true;
  }
}

/* We use here the quick sort. There is here no handling of doublons */
static size_t qsort_rows_size=0;

static int qsort_rows_compar(const void* q1, const void* q2)
{
  return (vector_compare(*((pkint_t**)q1), *((pkint_t**)q2),
			 (int)qsort_rows_size));
}

void matrix_sort_rows(matrix_t* const mat)
{
  if (!mat->_sorted){
    if (mat->nbrows>=6){
      qsort_rows_size = mat->nbcolumns;
      qsort((void*)mat->p, (size_t)mat->nbrows, sizeof(pkint_t*),
	    qsort_rows_compar);
      mat->_sorted = true;
    }
    else {
      matrix_isort_rows(mat);
    }
  }
}

/* This variant permutes also the saturation matrix together with the matrix.
   There is here no handling of doublons. */

typedef struct qsort_t {
  pkint_t* p;
  bitstring_t* satp;
} qsort_t;
static qsort_t* qsort_tab = NULL;

static int qsort_rows_with_sat_compar(const void* q1, const void* q2)
{
  return (vector_compare( ((qsort_t*)q1)->p, ((qsort_t*)q2)->p,
			  (int)qsort_rows_size));
}

void matrix_sort_rows_with_sat(matrix_t* const mat, satmat_t* const sat)
{
  int i;

  if (!mat->_sorted){
    if (mat->nbrows>=6){
      qsort_rows_size = mat->nbcolumns;
      qsort_tab = (qsort_t*)malloc(mat->nbrows * sizeof(qsort_t));
      for (i=0; i<mat->nbrows; i++){
	qsort_tab[i].p = mat->p[i];
	qsort_tab[i].satp = sat->p[i];
      }
      qsort(qsort_tab,
	    (size_t)mat->nbrows, sizeof(qsort_t),
	    qsort_rows_with_sat_compar);
      for (i=0; i<mat->nbrows; i++){
	mat->p[i] = qsort_tab[i].p;
	sat->p[i] = qsort_tab[i].satp;
      }
      free(qsort_tab);
      mat->_sorted = true;
    }
    else {
      matrix_isort_rows_with_sat(mat,sat);
    }
  }
}

/* %------------------------------------------------------------------------ */
/* \subsection{Addition of sorted rows} */
/* %------------------------------------------------------------------------ */

/* This function adds to a sorted matrix the rows of another sorted matrix
   and leaves the resulting matrix sorted. Identical rows are eliminated. The
   modified matrix is supposed to be big enough to store the new rows. */

void matrix_add_rows_sort(matrix_t* const mat, matrix_t* const cmat)
{
  int i,j,k,bound;

  if (mat->nbcolumns != cmat->nbcolumns){
    fprintf(stderr,
	    "matrix_add_rows_sort called with incompatible dimensions !\n");
    exit(2);
  }
  else if (mat->nbrows + cmat->nbrows > mat->_maxrows){
    fprintf(stderr,
	    "matrix_add_rows_sort called with a too small matrix");
    exit(2);
  }

  if (!mat->_sorted) matrix_sort_rows(mat);
  if (!cmat->_sorted) matrix_sort_rows(cmat);

  /* one adds the coefficients of cmat to mat */
  for (i=0; i<cmat->nbrows; i++){
    for (j=0; j<cmat->nbcolumns; j++){
      pkint_set(mat->p[mat->nbrows+i][j],cmat->p[i][j]);
    }
  }
  /*c now we fill pk_matrix_pkintpp */
  i = 0;
  j = mat->nbrows;
  k = 0;
  bound = mat->nbrows + cmat->nbrows;
  while (i < mat->nbrows && j < mat->nbrows + cmat->nbrows){
    int res = matrix_compare_rows(mat,i,j);
    if (res<=0){
      pk_matrix_pkintpp[k] = mat->p[i]; i++;
      if (res==0){
	bound--;
	pk_matrix_pkintpp[bound] = mat->p[j]; j++;
      }
    }
    else {
      pk_matrix_pkintpp[k] = mat->p[j]; j++;
    }
    k++;
  }
  /* Are there still constraints ? */
  if (i < mat->nbrows){
    do {
      pk_matrix_pkintpp[k] = mat->p[i];
      k++; i++;
    } while (i < mat->nbrows);
  }
  else {
    while (j < mat->nbrows + cmat->nbrows){
      pk_matrix_pkintpp[k] = mat->p[j];
      k++; j++;
    }
  }
  /*c fill mat->p with pk_matrix_pkintpp */
  for (i=0; i<mat->nbrows+cmat->nbrows; i++)
  mat->p[i] = pk_matrix_pkintpp[i];
  mat->nbrows = bound;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Merging} */
/* %------------------------------------------------------------------------ */

/* This function performs the same operation as the last one, but it is
   functional. */

matrix_t* matrix_merge_sort(matrix_t* mata, matrix_t* matb)
{
  int i,ia,ib,l;
  matrix_t* mat;
  if (mata->nbcolumns != matb->nbcolumns){
    fprintf(stderr,
	    "matrix_merge_sort called with incompatible dimensions !\n");
    exit(2);
  }
  if (!mata->_sorted) matrix_sort_rows(mata);
  if (!matb->_sorted) matrix_sort_rows(matb);

  mat = _matrix_alloc_int(mata->nbrows+matb->nbrows,mata->nbcolumns,true);
  i = 0;
  ia = 0;
  ib = 0;
  while (ia < mata->nbrows && ib < matb->nbrows) {
    int res = vector_compare(mata->p[ia],matb->p[ib],mat->nbcolumns);
    if (res<=0){
      for (l=0; l<mat->nbcolumns; l++)
	pkint_init_set(mat->p[i][l],mata->p[ia][l]);
      ia++;
      if (res==0) ib++;
    }
    else {
      for (l=0; l<mat->nbcolumns; l++)
	pkint_init_set(mat->p[i][l],matb->p[ib][l]);
      ib++;
    }
    i++;
  }
  /* does some constraint remain ? */
  if (ia < mata->nbrows) {
    do {
      for (l=0; l<mat->nbcolumns; l++)
	pkint_init_set(mat->p[i][l],mata->p[ia][l]);
      ia++; i++;
    } while (ia < mata->nbrows);
  } else {
    while (ib < matb->nbrows){
      for (l=0; l<mat->nbcolumns; l++)
	pkint_init_set(mat->p[i][l],matb->p[ib][l]);
      ib++; i++;
    }
  }
  /* initialize last rows of mat to zero */
  while (i<mat->nbrows){
    for (l=0; l<mat->nbcolumns; l++)
      pkint_init(mat->p[i][l]);
    i++;
  }
  mat->nbrows = i;
  return mat;
}


/* %======================================================================== */
/* \section{Linear tranformation} */
/* %======================================================================== */

/*
Variables are referenced by their \emph{real} index in the matrix.  There is
no check of bounds. $tab = [d,a_0,a_1,\ldots,a_n]$ represents the expression
$\frac{\sum_{i=0}^{n} a_i x_i}{d}$.
*/

/* %------------------------------------------------------------------------ */
/* \subsection{Single transformation: one variable and one expression} */
/* %------------------------------------------------------------------------ */

/* %------------------------------------------------------------------------ */
/* \subsubsection{Assignement of an expression to a variable} */
/* %------------------------------------------------------------------------ */

matrix_t* matrix_assign_variable(const matrix_t* const mat,
				 const int var, const pkint_t* const tab)
{
  int i,j;
  matrix_t* nmat = _matrix_alloc_int(mat->nbrows, mat->nbcolumns,false);
  if (pkint_cmp_ui(tab[0],1)==0){ /* d==1 => optimization */
    for (i=0; i<mat->nbrows; i++){
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
      /* columns != var */
      for (j=1; j<mat->nbcolumns; j++){
	if (j!=var){
	  pkint_init_set(nmat->p[i][j],mat->p[i][j]);
	}
      }
      /* var column */
      vector_product(&pk_matrix_prod, mat->p[i],tab,mat->nbcolumns);
      pkint_init_set(nmat->p[i][var],pk_matrix_prod);
      matrix_normalize_row(nmat,i);
    }
  } else { /* d != 1 */
    for (i=0; i<mat->nbrows; i++){
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
      /* columns != var */
      for (j=1; j<mat->nbcolumns; j++){
	if (j!=var){
	  pkint_init(nmat->p[i][j]);
	  pkint_mul(nmat->p[i][j],mat->p[i][j],tab[0]);
	}
      }
      /* var column */
      vector_product(&pk_matrix_prod, mat->p[i],tab,mat->nbcolumns);
      pkint_init_set(nmat->p[i][var],pk_matrix_prod);
      matrix_normalize_row(nmat,i);
    }
  }
  return nmat;
}

/* %------------------------------------------------------------------------ */
/* \subsubsection{Substitution of a variable by an expression} */
/* %------------------------------------------------------------------------ */

matrix_t* matrix_substitute_variable(const matrix_t* const mat,
				   const int var, const pkint_t* const tab)
{
  int i,j;
  matrix_t* nmat =  _matrix_alloc_int(mat->nbrows, mat->nbcolumns,false);
  if (pkint_cmp_ui(tab[0],1)==0){ /* d==1 => optimization */
    for (i=0; i<mat->nbrows; i++) {
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
      if (pkint_sgn(mat->p[i][var])) {
	/* columns != var */
	for (j=1; j<mat->nbcolumns; j++) {
	  if (j!=var){
	    pkint_init(nmat->p[i][j]);
	    pkint_mul(pk_matrix_prod,mat->p[i][var],tab[j]);
	    pkint_add(nmat->p[i][j],mat->p[i][j],pk_matrix_prod);
	  }
	}
	/* var column */
	pkint_init(nmat->p[i][var]);
	pkint_mul(nmat->p[i][var],mat->p[i][var],tab[var]);
	matrix_normalize_row(nmat,i);
      }
      else {
	for (j=1; j<mat->nbcolumns; j++)
	  pkint_init_set(nmat->p[i][j],mat->p[i][j]);
      }
    }
  } else { /* d != 1 */
    for (i=0; i<mat->nbrows; i++) {
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
      if (pkint_sgn(mat->p[i][var])) {
	/* columns != var */
	for (j=1; j<mat->nbcolumns; j++) {
	  if (j!=var){
	    pkint_init(nmat->p[i][j]);
	    pkint_mul(nmat->p[i][j],mat->p[i][j],tab[0]);
	    pkint_mul(pk_matrix_prod,mat->p[i][var],tab[j]);
	    pkint_add(nmat->p[i][j],nmat->p[i][j],pk_matrix_prod);
	  }
	}
	/* var column */
	pkint_init(nmat->p[i][var]);
	pkint_mul(nmat->p[i][var],mat->p[i][var],tab[var]);
	matrix_normalize_row(nmat,i);
      }
      else {
	for (j=1; j<mat->nbcolumns; j++)
	  pkint_init_set(nmat->p[i][j],mat->p[i][j]);
      }
    }
  }
  return nmat;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Simultaneous transformation: several variables and expressions} */
/* %------------------------------------------------------------------------ */

/* The list of pair (variable,expr) is given by an array of type
   \verb|equation_t|. IMPORTANT: the array is supposed to be sorted in
   increasing order w.r.t. the field \verb|dim|. */

/* %------------------------------------------------------------------------ */
/* \subsubsection{Assignement by an array of equations} */
/* %------------------------------------------------------------------------ */

matrix_t* matrix_assign_variables(const matrix_t* const mat,
				 const equation_t * eqn,
				 const int size)
{
  int i,j,eindex;
  matrix_t* nmat = _matrix_alloc_int(mat->nbrows, mat->nbcolumns,false);
  pkint_t den;

  if (size==0){
    fprintf(stderr,"Polka: matrix.c: matrix_assign_variables: error: array of null size given\n");
    exit(-1);
  }
  for (i=0; i<size; i++){
    if (eqn[i].var < 1 || eqn[i].var >= mat->nbcolumns){
      fprintf(stderr,
	      "Polka: matrix.c: matrix_assign_variables: invalid value for eqn[%d].var=%d\n",
	      i,eqn[i].var);
      exit(-1);
    }
  }
  /* Computing common denominator */
  pkint_init_set(den,eqn[0].expr[0]);
  for (i=1; i<size; i++){
    pkint_mul(den,den,eqn[i].expr[0]);
  }

  if (pkint_cmp_ui(den,1)!=0){
    /* General case */
    pkint_t* vden = vector_alloc(size);
    for (i=0; i<size; i++){
      pkint_divexact(vden[i],den,eqn[i].expr[0]);
    }
    /* Column 0: copy */
    for (i=0; i<mat->nbrows; i++){
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
    }
    /* Other columns */
    eindex = 0;
    for (j=1; j<mat->nbcolumns; j++){
      if (eindex < size && eqn[eindex].var == j){
	/* We are on a assigned column */
	for (i=0; i<mat->nbrows; i++){ /* For each row */
	  vector_product(&pk_matrix_prod, mat->p[i], eqn[eindex].expr,mat->nbcolumns);
	  pkint_mul(pk_matrix_prod, pk_matrix_prod, vden[eindex]);
	  /* Put the result */
	  pkint_init_set(nmat->p[i][j], pk_matrix_prod);
	}
	eindex++;
      }
      else {
	/* We are on a normal column */
	for (i=0; i<mat->nbrows; i++){ /* For each row */
	  pkint_init(nmat->p[i][j]);
	  pkint_mul(nmat->p[i][j],mat->p[i][j],den);
	}
      }
    }
    vector_free(vden,size);
  }
  else {
    /* Special case: all denominators are 1 */
    /* Column 0: copy */
    for (i=0; i<mat->nbrows; i++){
      pkint_init_set(nmat->p[i][0],mat->p[i][0]);
    }
    /* Other columns */
    eindex = 0;
    for (j=1; j<mat->nbcolumns; j++){
      if (eindex < size && eqn[eindex].var == j){
	/* We are on a assigned column */
	for (i=0; i<mat->nbrows; i++){ /* For each row */
	  vector_product(&pk_matrix_prod, mat->p[i], eqn[eindex].expr,mat->nbcolumns);
	  pkint_init_set(nmat->p[i][j],pk_matrix_prod);
	}
	eindex++;
      }
      else {
	/* We are on a normal column */
	for (i=0; i<mat->nbrows; i++){ /* For each row */
	  pkint_init_set(nmat->p[i][j],mat->p[i][j]);
	}
      }
    }
  }
  pkint_clear(den);
  for (i=0; i<mat->nbrows; i++){
    matrix_normalize_row(nmat,i);
  }

  return nmat;
}

/* %------------------------------------------------------------------------ */
/* \subsubsection{Substitution by an array of equations} */
/* %------------------------------------------------------------------------ */

matrix_t* matrix_substitute_variables(const matrix_t* const mat,
				 const equation_t * eqn,
				 const int size)
{
  int i,j,eindex;
  matrix_t* nmat = matrix_alloc(mat->nbrows, mat->nbcolumns,false);
  pkint_t den;

  if (size==0){
    fprintf(stderr,"Polka: matrix.c: matrix_substitute_variables: error: array of null size given\n");
    exit(-1);
  }
  for (i=0; i<size; i++){
    if (eqn[i].var < 1 || eqn[i].var >= mat->nbcolumns){
      fprintf(stderr,
	      "Polka: matrix.c: matrix_substitute_variables: invalid value for eqn[%d].var=%d\n",
	      i,eqn[i].var);
      exit(-1);
    }
  }
  /* Computing common denominator */
  pkint_init_set(den,eqn[0].expr[0]);
  for (i=1; i<size; i++){
    pkint_mul(den,den,eqn[i].expr[0]);
  }

  if (pkint_cmp_ui(den,1)!=0){
    /* General case */
    pkint_t* vden = vector_alloc(size);
    for (i=0; i<size; i++){
      pkint_divexact(vden[i],den,eqn[i].expr[0]);
    }
    /* For each row */
    for (i=0; i<mat->nbrows; i++) {
      /* Column 0 */
      pkint_set(nmat->p[i][0],mat->p[i][0]);
      /* Other columns */
      /* First, copy the row and sets to zero substituted variables */
      eindex = 0;
      for (j=1; j<mat->nbcolumns; j++){
	if (eindex < size && eqn[eindex].var == j)
	  eindex++;
	else
	  pkint_mul(nmat->p[i][j],mat->p[i][j],den);
      }
      /* Second, add things coming from substitution */
      for (j=1; j<mat->nbcolumns; j++){
	for (eindex=0; eindex<size; eindex++){
	  if (pkint_sgn(mat->p[i][eqn[eindex].var])) {
	    pkint_mul(pk_matrix_prod,
		      mat->p[i][eqn[eindex].var],
		      eqn[eindex].expr[j]);
	    pkint_mul(pk_matrix_prod,pk_matrix_prod, vden[eindex]);
	    pkint_add(nmat->p[i][j],nmat->p[i][j],pk_matrix_prod);
	  }
	}
      }
    }
    vector_free(vden,size);
  }
  else {
    /* Special case: all denominators are 1 */
    /* For each row */
    for (i=0; i<mat->nbrows; i++) {
      /* Column 0 */
      pkint_set(nmat->p[i][0],mat->p[i][0]);
      /* Other columns */
      /* First, copy the row and sets to zero substituted variables */
      eindex = 0;
      for (j=1; j<mat->nbcolumns; j++){
	if (eindex < size && eqn[eindex].var == j)
	  eindex++;
	else
	  pkint_set(nmat->p[i][j],mat->p[i][j]);
      }
      /* Second, add things coming from substitution */
      for (j=1; j<mat->nbcolumns; j++){
	for (eindex=0; eindex<size; eindex++){
	  if (pkint_sgn(mat->p[i][eqn[eindex].var])) {
	    pkint_mul(pk_matrix_prod,
		      mat->p[i][eqn[eindex].var],
		      eqn[eindex].expr[j]);
	    pkint_add(nmat->p[i][j],nmat->p[i][j],pk_matrix_prod);
	  }
	}
      }
    }
  }
  pkint_clear(den);
  for (i=0; i<mat->nbrows; i++){
    matrix_normalize_row(nmat,i);
  }

  return nmat;
}

/* %======================================================================== */
/* \section{Change of dimensions} */
/* %======================================================================== */

/* Addition or removal of dimensions at the end.
   Scales verb|vector_add_dimensions| to matrices.
*/

matrix_t* matrix_add_dimensions(const matrix_t* mat, int dimsup)
{
  int i;
  matrix_t* nmat;

  nmat = matrix_alloc(mat->nbrows, mat->nbcolumns+dimsup, matrix_is_sorted(mat));
  for (i=0; i < mat->nbrows; i++)
    vector_add_dimensions(nmat->p[i],mat->p[i],mat->nbcolumns,dimsup);
  return nmat;
}

/* Addition of new dimensions anywhere.
   Scales verb|vector_add_dimensions_multi| to matrices.
*/
matrix_t* matrix_add_dimensions_multi(const matrix_t* mat,
				      const dimsup_t* tab, int size)
{
  int i,dimsup;
  matrix_t* nmat;

  /* Checks the input */
  dimsup = check_add_dimsup_of_multi(mat->nbcolumns,tab,size);
  nmat = matrix_alloc(mat->nbrows, mat->nbcolumns+dimsup, matrix_is_sorted(mat));
  for (i=0; i < mat->nbrows; i++)
    vector_add_dimensions_multi(nmat->p[i],mat->p[i],mat->nbcolumns,tab,size);
  return nmat;
}

/* Removal of dimensions anywhere.
   Scales verb|vector_remove_dimensions_multi| to matrices.
*/
matrix_t* matrix_remove_dimensions_multi(const matrix_t* mat,
					 const dimsup_t* tab, int size)
{
  int i,dimsup;
  matrix_t* nmat;

  /* Checks the input */
  dimsup = check_remove_dimsup_of_multi(mat->nbcolumns,tab,size);
  nmat = matrix_alloc(mat->nbrows, mat->nbcolumns-dimsup, false);
  for (i=0; i < mat->nbrows; i++)
    vector_remove_dimensions_multi(nmat->p[i],mat->p[i],mat->nbcolumns,tab,size);
  return nmat;
}

/* Addition and permutation of dimensions.
   Scales verb|vector_add_permute_dimensions| to matrices.
*/
matrix_t* matrix_add_permute_dimensions(const matrix_t* mat,
					int dimsup,
					const int * permutation)
{
  int i;
  matrix_t* nmat;
  if (dimsup<0){
    fprintf(stderr,"Polka: matrix.c: matrix_add_permute_dimensions: negative dimsup argument\n");
    exit(-1);
  }
  nmat = matrix_alloc(mat->nbrows, mat->nbcolumns+dimsup, false);
  for (i=0; i < mat->nbrows; i++){
    vector_add_permute_dimensions(nmat->p[i],
				  mat->p[i],mat->nbcolumns,
				  dimsup,permutation);
  }
  return nmat;
}

/* Permutation and removal of dimensions.
   Scales verb|vector_permute_remove_dimensions| to matrices.
*/
matrix_t* matrix_permute_remove_dimensions(const matrix_t* mat,
					int dimsup,
					const int * permutation)
{
  int i;
  matrix_t* nmat;
  if (dimsup<0){
    fprintf(stderr,"Polka: matrix.c: matrix_permute_remove_dimensions: negative dimsup argument\n");
    exit(-1);
  }
  nmat = matrix_alloc(mat->nbrows, mat->nbcolumns-dimsup, false);
  for (i=0; i < mat->nbrows; i++){
    vector_permute_remove_dimensions(nmat->p[i],
				     mat->p[i],mat->nbcolumns,
				     dimsup,permutation);
  }
  return nmat;
}

/*% The matrix \verb-mat- is supposed to be minimized. As a consequence, */
/*% one sort separately the bidirectionnal and unidirectionnal rows. In */
/*% the notation, we suppose that the \verb-this- is a constraint matrix. As */
/*% a consequence, the saturation matrix \verb-sat- has columns indexed by */
/*% constraints (type satC),  */

/*% void matrix_sort_rows_with_satcols(matrix_t* const mat, satmat_t* const sat, const int nbeq, const int nbline) */
/*% { */
/*%   /\* bidirectionnals *\/ */
/*%   { */
/*%     int i,j; */
/*%     pkint_t* row; */

/*%     for (i=1; i<nbeq; i++){ */
/*%       row = mat->p[i]; */
/*%       j = i; */
/*%       while (j > 0 && vector_compare(mat->p[j-1],row,mat->nbcolumns) > 0){ */
/*%	mat->p[j] = mat->p[j-1]; */
/*%	j--; */
/*%       } */
/*%       mat->p[j] = row; */
/*%     } */
/*%   } */

/*%   /\* others *\/ */
/*%   { */
/*%     int l,s; */
/*%     bitindex_t i,j,j1,nbrows; */
/*%     pkint_t* row; */

/*%     nbrows = bitindex_init(mat->nbrows); */
/*%     i = bitindex_init(nbeq+1); */
/*%     while(i.index < nbrows.index) { */
/*%       row = mat->p[i.index]; */
/*%       /\* on recopie dans un tampon la colonne correspondant a row de sat *\/ */
/*%       for(l=nbline; l<sat->nbrows; l++){ */
/*%	pk_matrix_bitstringp[l] = satmat_get(sat,l,i); */
/*%       } */
/*%       j = i;  */
/*%       j1 = i; */
/*%       s = -1; */
/*%       while (j.index > nbeq){ */
/*%	s = vector_compare(mat->p[j.index-1],row,mat->nbcolumns); */
/*%	if (s<=0) break; */
/*%	bitindex_dec(&j1); /\* j1 = j - 1 *\/ */
/*%	mat->p[j.index] = mat->p[j1.index]; */
/*%	satmat_assign_col(sat,nbline,j,j1); */
/*%	j = j1; */
/*%       } */
/*%       if (s==0){ /\* doublons !  *\/ */
/*%	/\* on restaure *\/ */
/*%	while (j.index < i.index){ */
/*%	  bitindex_inc(&j1); /\* j1 = j + 1 *\/ */
/*%	  mat->p[j.index] = mat->p[j1.index]; */
/*%	  satmat_assign_col(sat,nbline,j,j1); */
/*%	  j = j1; */
/*%	} */
/*%	mat->p[i.index] = row; */
/*%	/\* et on elimine la ligne *\/ */
/*%	bitindex_dec(&nbrows); */
/*%	matrix_exch_rows(mat,i.index,nbrows.index); */
/*%	satmat_assign_col(sat,nbline,i,nbrows); */
/*%       }  */
/*%       else { /\* on insere *\/ */
/*%	mat->p[j.index] = row; */
/*%	for(l=nbline; l<sat->nbrows; l++){ */
/*%	  if (pk_matrix_bitstringp[l]) satmat_set(sat,l,j); else satmat_clr(sat,l,j); */
/*%	} */
/*%	bitindex_inc(&i); */
/*%       } */
/*%     } */
/*%     mat->nbrows = nbrows.index; */
/*%   } */
/*%   mat->_sorted = true; */
/*% } */
