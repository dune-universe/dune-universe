/*% $Id: poly.c,v 1.6 2004/02/27 10:34:10 bjeannet Exp $ */

/*% Operations on polyhedra. */

#include <stdio.h>
#include <assert.h>

#include "poly.h"
#include "internal.h"
#include "cherni.h"

static bool check(const poly_t* poly);
static void obtain_satC(const poly_t* poly);
static void obtain_satF(const poly_t* poly);
static void obtain_sorted_C(const poly_t* poly);
static void obtain_sorted_F(const poly_t* poly);
static void obtain_sorted_C_with_satC(const poly_t* poly);
static void obtain_sorted_F_with_satF(const poly_t* poly);
static tbool frames_versus_constraint(const matrix_t* F, const pkint_t* tab);
static bool is_frame_included_in_constraints(const pkint_t* tab, const matrix_t* C);

/* %======================================================================== */
/* \section[Basic operations]
   {Basic operations: creation, destruction, copying and printing} */
/* %======================================================================== */

/* This \emph{internal} function allocates a polyhedron and fills its records
   with null values. */
poly_t* _poly_alloc(int dim){
  poly_t* po = (poly_t*)malloc(sizeof(poly_t));
  po->C = po->F = 0;
  po->satC = po->satF = 0;
  po->dim = dim;
  po->nbeq = 0;
  po->nbline = 0;
  return po;
}

/* Finalization function for polyhedra, which frees
   recursively the members of the structure. */
void poly_free(poly_t* po)
{
  if (po->C) matrix_free(po->C);
  if (po->F) matrix_free(po->F);
  if (po->satC) satmat_free(po->satC);
  if (po->satF) satmat_free(po->satF);
  free(po);
}

/* Duplicate (recursively) a polyhedron. */
poly_t* poly_copy(const poly_t* po)
{
  poly_t* npo = _poly_alloc(po->dim);
  npo->C = po->C ? matrix_copy(po->C) : 0;
  npo->F = po->F ? matrix_copy(po->F) : 0;
  npo->satC = po->satC ? satmat_copy(po->satC) : 0;
  npo->satF = po->satF ? satmat_copy(po->satF) : 0;
  npo->nbeq = po->nbeq;
  npo->nbline = po->nbline;
  return npo;
}

/* Raw printing function. */
void poly_print(const poly_t* po)
{
  if (!po->C && !po->F)
    printf("empty polyhedron of dim %d\n",po->dim);
  else {
    printf("polyhedron of dim %d\n",po->dim);
    if (po->C){
      printf("Constraints: ");
      matrix_print(po->C);
    }
    if (po->F){
      printf("Frames: ");
      matrix_print(po->F);
    }
    if (po->satC){
      printf("satC: ");
      satmat_print(po->satC);
    }
    if (po->satF){
      printf("satF: ");
      satmat_print(po->satF);
    }
  }
}

/* Minimization function.

This function minimizes if not already done the given polyhedron. */
void poly_minimize(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if ((po->C && po->F) || (!po->C && !po->F))
    return;
  else {
    if (po->C){
      cherni_minimize(true, &po->C,
		      &po->F, &po->satF, &po->nbeq, &po->nbline);
    }
    else {
      cherni_minimize(false, &po->F,
		      &po->C, &po->satC, &po->nbline, &po->nbeq);
    }
    return;
  }
}

/* Canonicalization function.

This function implies minimization. It normalizes set of equalities and lines.
In addition, for polyhedra with strict constraints, it put the constraints into
normalized form, where epsilon has the coefficient -1 and all the other
coefficients are normalized as if there were no epsilon coefficient.
*/

void poly_canonicalize(const poly_t* poly)
{
  int i;
  bool change;

  poly_t* po = (poly_t*)poly;

  if (po->C==0 && po->F==0)
    return;

  if (!polka_strict){
    poly_minimize(po);
  }
  else {
    // Obtain C
    if (!po->C) poly_minimize(po);
    matrix_t* C = po->C;
    change = false;

    // Normalize strict constraints of C
    for (i=0; i<C->nbrows; i++){
      int strict = pkint_sgn(C->p[i][polka_eps]);
      if (strict<0){
	change = true;
	pkint_set_si(C->p[i][polka_eps],0);
	vector_normalize(C->p[i],C->nbcolumns);
	pkint_set_si(C->p[i][polka_eps],-1);
      }
    }

    // If there were some change, perform a new minimization from normalized C
    if (change){
      C->_sorted = false;
      if (po->F) matrix_free(po->F);
      if (po->satC) satmat_free(po->satC);
      if (po->satF) satmat_free(po->satF);
      po->F = 0;
      po->satC = po->satF = 0;
    }
    poly_minimize(po);
  }
  if (po->C && po->F){
    int rank;
    /* 1. Gauss elimination on lines */
    rank = gauss(po->F, po->nbline); /* gauss pivot to simplify lines */
    assert(rank==po->nbline);
    backsubstitute(po->F, po->nbline);
    /* 2. Gauss elimination on equations */
    rank = gauss(po->C, po->nbeq); /* gauss pivot to simplify lines */
    assert(rank==po->nbeq);
    backsubstitute(po->C, po->nbeq);
  }
}

/* %======================================================================== */
/* \section{Access functions} */
/* %======================================================================== */

const matrix_t* poly_constraints(const poly_t* po) { return po->C; }
const matrix_t* poly_frames(const poly_t* po) { return po->F; }
const satmat_t* poly_satC(const poly_t* po){
  poly_minimize(po);
  obtain_satC(po);
  return po->satC;
}
const satmat_t* poly_satF(const poly_t* po){
  poly_minimize(po);
  obtain_satF(po);
  return po->satC;
}

int poly_dimension(const poly_t* po) { return po->dim; }
int poly_nbequations(const poly_t* po){
  poly_minimize(po);
  return po->nbeq;
}
int poly_nblines(const poly_t* po){
  poly_minimize(po);
  return po->nbline;
}
int poly_nbconstraints(const poly_t* po){
  poly_minimize(po);
  return (po->C==0) ? 0 : po->C->nbrows;
}
int poly_nbframes(const poly_t* po){
  poly_minimize(po);
  return (po->F==0) ? 0 : po->F->nbrows;
}

/* %======================================================================== */
/* \section{Basic constructors} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Empty \& Universe polyhedra} */
/* %------------------------------------------------------------------------ */

/* Empty polyhedron.

The empty polyhedron is just defined by the absence of both
constraints matrix and frames matrix. */

poly_t* poly_empty(int dim){
  return _poly_alloc(dim);
}

/* Universe polyhedron. */

/*
If there is no $\epsilon$-dimension, the universe polyhedron has as
constraint $\xi \geq 0$ and as frames the lines of direction $x_i$ for $0
\leq i < pdim$, and the ray of direction $\xi$.

If there is an $\epsilon$ dimension, the universe polyhedron have as
constraints $0\leq\epsilon\leq\xi$ and as frames the lines of
direction $x_i$ for $0 \leq i < pdim$, and the rays of direction $\xi$
and $\xi+\epsilon$.
*/

poly_t* poly_universe(int dim)
{
  int i;
  poly_t* po;


  if (dim<=0 || dim>polka_maxcolumns-polka_dec) {
    fprintf(stderr,"poly: cannot create polyhedron with null, negative dimension, or dimension greater than polka_maxcolumns-polka_dec\n");
    exit(-1);
  }
  po = _poly_alloc(dim);

  po->C = matrix_alloc(polka_dec-1, polka_dec+dim,true);
  po->F = matrix_alloc(polka_dec+dim-1,polka_dec+dim,true);
  /* We have to ensure that the matrices are really sorted */
  po->satC = satmat_alloc(polka_dec+dim-1,polka_dec-1);
  po->satF = 0;
  po->nbeq = 0;
  po->nbline = dim;

  /* lines $x_i$ */
  for(i=0; i<dim; i++){
    pkint_set_ui(po->F->p[i][polka_dec+dim-1-i],1);
  }
  if (polka_strict){
    assert(polka_cst==1 && polka_eps==2);
    /* rays xi and xi+epsilon */
    pkint_set_ui(po->F->p[dim][0],1);
    pkint_set_ui(po->F->p[dim][polka_cst],1);
    pkint_set_ui(po->F->p[dim+1][0],1);
    pkint_set_ui(po->F->p[dim+1][polka_cst],1);
    pkint_set_ui(po->F->p[dim+1][polka_eps],1);
    /* constraints epsilon and xi-epsilon*/
    pkint_set_ui(po->C->p[0][0],1);
    pkint_set_ui(po->C->p[0][polka_eps],1);
    pkint_set_ui(po->C->p[1][0],1);
    pkint_set_ui(po->C->p[1][polka_cst],1);
    pkint_set_si(po->C->p[1][polka_eps],(-1));
    /* saturation matrix */
    po->satC->p[dim][0] = bitstring_msb >> 1;
    po->satC->p[dim+1][0] = bitstring_msb;
  }
  else {
    /* ray xi */
    pkint_set_ui(po->F->p[dim][0],1);
    pkint_set_ui(po->F->p[dim][polka_cst],1);
    /* constraint \xi \geq 0 */
    pkint_set_ui(po->C->p[0][0],1);
    pkint_set_ui(po->C->p[0][polka_cst],1);
    /* saturation matrix */
    po->satC->p[dim][0] = bitstring_msb;
  }

  return po;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Creation from constraints or frames} */
/* %------------------------------------------------------------------------ */

/*
The parameter is integrated into the polyhedron (no copy).

If \verb-polka_strict==false- matrices of constraints are supposed to
contain $\xi \leq 0$, else they have to contain both $\xi\leq\epsilon$
and $\epsilon\leq 0$, unless if this is implied by the other
constraints.
*/

poly_t* poly_of_constraints(matrix_t* mat)
{
  poly_t* po;

  if (mat->nbcolumns<=0 || mat->nbcolumns > polka_maxcolumns) {
    fprintf(stderr,"poly: cannot create polyhedron with null, negative dimension, or dimension greater than polka_maxcolumns-polka_dec\n");
    exit(-1);
  }
  po = _poly_alloc((mat->nbcolumns-polka_dec));
  po->C = mat;
  return po;
}

poly_t* poly_of_frames(matrix_t* mat)
{
  poly_t* po;

  if (mat->nbcolumns<=0 || mat->nbcolumns > polka_maxcolumns) {
    fprintf(stderr,"poly: cannot create polyhedron with null, negative dimension, or dimension greater than polka_maxcolumns-polka_dec\n");
    exit(-1);
  }
  po = _poly_alloc((mat->nbcolumns-polka_dec));
  po->F = mat;
  return po;
}

/* %======================================================================== */
/* \section{Small internal functions} */
/* %======================================================================== */

/* Check that the two polyhedra have the same dimension. */
static void check_dims(const poly_t* pa, const poly_t* pb, const char* name)
{
  if (pa->dim != pb->dim){
    fflush(stdout);
    fprintf(stderr,
	    "poly: Incompatible dimensions in %s function\n",name);
    poly_print(pa); poly_print(pb);
    abort();
  }
}
/* Check the coherence of a polyhedron description. */
static bool check(const poly_t* po)
{
  if (po->C==0 || po->F==0)
    return po->nbeq==0 && po->nbline==0 && po->satC==0 && po->satF==0;
  else {
    if (po->satC && po->satF){
      return (cherni_checksatmat(true,po->C,po->F,po->satC)
	      && cherni_checksatmat(false,po->F,po->C,po->satF)
	      && cherni_checksat(true,po->C,po->nbeq,po->F,po->nbline,po->satC));
    }
    else {
      if (po->satC){
	return (cherni_checksatmat(true,po->C,po->F,po->satC)
		&& cherni_checksat(true,po->C,po->nbeq,po->F,po->nbline,po->satC));
      }
      else {
	return (po->satF!=0 && cherni_checksatmat(false,po->F,po->C,po->satF)
		&& cherni_checksat(false,po->F,po->nbline,po->C,po->nbeq,po->satF));
      }
    }
  }
}

/* Get the saturation matrix in the right orientation. */
static void obtain_satC(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (!po->satC)
    po->satC = satmat_transpose(po->satF,po->F->nbrows);
}
static void obtain_satF(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (!po->satF)
    po->satF = satmat_transpose(po->satC,po->C->nbrows);
}

/* Sort the constraint matrix while keeping the saturation matrix OK. */
static void obtain_sorted_C_with_satC(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (matrix_is_sorted(po->C)){
    if (!po->satC)
      po->satC = satmat_transpose(po->satF,po->F->nbrows);
  }
  else {
    if (po->satF){
      if (po->satC){ satmat_free(po->satC); po->satC = 0; }
      matrix_sort_rows_with_sat(po->C,po->satF);
    }
    else {
      po->satF = satmat_transpose(po->satC,po->C->nbrows);
      satmat_free(po->satC); po->satC = 0;
      matrix_sort_rows_with_sat(po->C,po->satF);
    }
    po->satC = satmat_transpose(po->satF,po->F->nbrows);
  }
}

/* Sort the generator matrix while keeping the saturation matrix OK. */
static void obtain_sorted_F_with_satF(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (matrix_is_sorted(po->F)){
    if (!po->satF)
      po->satF = satmat_transpose(po->satC,po->C->nbrows);
  }
  else {
    if (po->satC){
      if (po->satF){ satmat_free(po->satF); po->satF = 0; }
      matrix_sort_rows_with_sat(po->F,po->satC);
    }
    else {
      po->satC = satmat_transpose(po->satF,po->F->nbrows);
      satmat_free(po->satF); po->satF = 0;
      matrix_sort_rows_with_sat(po->F,po->satC);
    }
    po->satF = satmat_transpose(po->satC,po->C->nbrows);
  }
}

/* Sort the constraint matrix, and keep the saturation matrix if not
   transposition is required.  */
static void obtain_sorted_C(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (!matrix_is_sorted(po->C)){
    if (po->satF){
      if (po->satC){ satmat_free(po->satC); po->satC = 0; }
      matrix_sort_rows_with_sat(po->C,po->satF);
    }
    else if (po->satC){
      po->satF = satmat_transpose(po->satC,po->C->nbrows);
      satmat_free(po->satC); po->satC = 0;
      matrix_sort_rows_with_sat(po->C,po->satF);
    }
    else {
      matrix_sort_rows(po->C);
    }
  }
}

/* Sort the generator matrix, and keep the saturation matrix if not
   transposition is required.  */
static void obtain_sorted_F(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;
  if (!matrix_is_sorted(po->F)){
    if (po->satC){
      if (po->satF){ satmat_free(po->satF); po->satF = 0; }
      matrix_sort_rows_with_sat(po->F,po->satC);
    }
    else if (po->satF){
      po->satC = satmat_transpose(po->satF,po->F->nbrows);
      satmat_free(po->satF); po->satF = 0;
      matrix_sort_rows_with_sat(po->F,po->satC);
    }
    else {
      matrix_sort_rows(po->F);
    }
  }
}

/* %======================================================================== */
/* \section{Internal operations} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Does a matrix of frames satisfy a constraint ?} */
/* %------------------------------------------------------------------------ */

typedef enum tsign_t {
  tsign_bottom = 0,
  tsign_gt = 1,
  tsign_eq = 2,
  tsign_lt = 3,
  tsign_geq = 4,
  tsign_leq = 5,
  tsign_top = 6
} tsign_t;

static bool tsign_is_leq(tsign_t x, tsign_t y)
{
  if (x==y || y==tsign_top) return true;
  switch(x){
  case tsign_bottom: return true;
  case tsign_gt: return (y==tsign_geq);
  case tsign_eq: return (y==tsign_geq || y==tsign_leq);
  case tsign_lt: return (y==tsign_leq);
  default: return false;
  }
}

static tsign_t tsign_union(tsign_t x, tsign_t y)
{
  if (x==y || x==tsign_top || y==tsign_bottom) return x;
  if (y==tsign_top || x==tsign_bottom) return y;
  /* Now, we know that x and y are not equal and different
     from top and bottom */
  switch(x){
  case tsign_gt:
    return
      (y==tsign_eq || y==tsign_geq)
      ?
      tsign_geq
      :
      tsign_top;
  case tsign_eq:
    return
      (y==tsign_gt || y==tsign_geq)
      ?
      tsign_geq
      :
      (y==tsign_lt || y==tsign_leq)
      ?
      tsign_leq
      :
      tsign_top;
  case tsign_lt:
    return
      (y==tsign_eq || y==tsign_leq)
      ?
      tsign_leq
      :
      tsign_top;
  case tsign_geq:
    return
      (y==tsign_eq || y==tsign_gt)
      ?
      tsign_geq
      :
      tsign_top;
  case tsign_leq:
    return
      (y==tsign_eq || y==tsign_lt)
      ?
      tsign_leq
      :
      tsign_top;
  default:
    fprintf(stderr,"poly.c: tsign_union: internal error\n");
    exit(1);
  }
}

/*
\verb|F| is suppposed to be a valid matrix of ray (i.e., corresponding
to a non empty polyhedron.

In case of inequality the result has the following meaning:
\begin{itemize}
\item \verb-tbool_top-: all frames belongs to the hyperplane
  defined by the constraint and the constraint is non-strict;
\item \verb-tbool_true-: all frames satisfies the constraint but do not
  verify the preceding property;
\item \verb-tbool_false-: no frame satisfies the constraint;
\item \verb-tbool_bottom-: default case.
\end{itemize}
In case of equality, the two possible results are \verb-tbool_top- and
\verb-tbool_bottom-.
*/

tbool frames_versus_constraint(const matrix_t* F, const pkint_t* tab)
{
  int i;

  if (pkint_sgn(tab[0])==0){
    /* 1. constraint is an equality */
    for (i=0; i<F->nbrows; i++){
      vector_product_strict(&pk_poly_prod,F->p[i],tab,F->nbcolumns);
      if (pkint_sgn(pk_poly_prod)) return tbool_bottom;
    }
    return tbool_top;
  }
  else {
    /* 2. constraint is an inequality */
    bool is_strict; /* Indicates wether the constraint is strict */
    int sign;      /* sign of the scalar product */
    tsign_t tsign; /* idem */
    tsign_t tsign_all,tsign_eps,tsign_xi;

    is_strict = polka_strict && (pkint_sgn(tab[polka_eps]) < 0);
    tsign_all = tsign_eps = tsign_xi = tsign_bottom;

    for (i=0; i<F->nbrows; i++){
      vector_product_strict(&pk_poly_prod,F->p[i],tab,F->nbcolumns);
      sign = pkint_sgn(pk_poly_prod);

      if (pkint_sgn(F->p[i][0])==0){
	/* line */
	if (sign!=0) return tbool_bottom;
      }
      else {
	tsign = (sign==0 ? tsign_eq :
		 sign>0 ? tsign_gt :
		 tsign_lt);

	if (pkint_sgn(F->p[i][polka_cst])>0){
	  /* vertex $\xi>0$ */
	  tsign_xi = tsign_union(tsign_xi,tsign);
	}
	if (is_strict && pkint_sgn(F->p[i][polka_eps])>0){
	  /* ray with $\epsilon>0$ */
	  tsign_eps = tsign_union(tsign_eps,tsign);
	}
	tsign_all = tsign_union(tsign_all,tsign);
	if (tsign_all==tsign_top) return tbool_bottom;
      }
    }
    if (is_strict){
      return
	(tsign_all==tsign_gt
	 ||
	 (tsign_all==tsign_geq
	  &&
	  (tsign_xi==tsign_gt || tsign_eps==tsign_gt)))
	?
	tbool_true
	:
	(tsign_all==tsign_leq || tsign_all==tsign_lt || tsign_all==tsign_eq)
	?
	tbool_false
	:
	tbool_bottom;
    }
    else {
      return
	(tsign_all==tsign_eq)
	?
	tbool_top
	:
	(tsign_all==tsign_geq || tsign_all==tsign_gt)
	?
	tbool_true
	:
	(tsign_all==tsign_lt
	 ||
	 (tsign_all==tsign_leq
	  &&
	  (tsign_xi==tsign_lt || tsign_eps==tsign_lt)))
	?
	tbool_false
	:
	tbool_bottom;
    }
  }
}

/* %------------------------------------------------------------------------ */
/* \subsection{Does a frame satisfy a matrix of constraints ?} */
/* %------------------------------------------------------------------------ */

/* Return \verb-true- if the frame satisfies all the constraints,
   \verb-false- otherwise. The frame is implicitly projected on
   $\epsilon=0$, and the projection is supposed to be not null. */

bool is_frame_included_in_constraints(const pkint_t* tab, const matrix_t* C)
{
  int i,s;

  bool is_ray = pkint_sgn(tab[0]);
  bool res = true;

  for (i=0; i<C->nbrows; i++){
    vector_product_strict(&pk_poly_prod,C->p[i],tab,C->nbcolumns);
    s = pkint_sgn(pk_poly_prod);
    if (s < 0) { res = false; break; }
    if (is_ray && pkint_sgn(C->p[i][0])){
       /* we have a ray and an inequality */
      if (s==0 && polka_strict && pkint_sgn(C->p[i][polka_eps]) < 0){
	res = false; break;
      }
    }
    else {
      /* we have an equality or a line */
      if (s!=0){ res = false; break; }
    }
  }
  return res;
}

/* %======================================================================== */
/* \section{Predicates} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Minimality, emptiness and ``Universness''} */
/* %------------------------------------------------------------------------ */

bool poly_is_minimal(const poly_t* po)
{
  return ((po->C && po->F) || (!po->C && !po->F));
}

bool poly_is_empty(const poly_t* po)
{
  if (po->F)
    return false;
  else {
    if (po->C){
      poly_minimize(po);
      return (!po->C && !po->F);
    }
    else
      return true;
  }
}

tbool poly_is_empty_lazy(const poly_t* po)
{
  if (po->F)
    return tbool_false;
  else
    if (po->C)
      return tbool_bottom;
    else
      return tbool_true;
}

bool poly_is_universe(const poly_t* po)
{
  if (!po->C && !po->F)
    return false;
  else {
    if (!po->C || !po->F){
      poly_minimize(po);
      if (!po->C) /* The polyhedron is empty */
	return false;
    }
    /* In this case, only one or two constraint */
    return (po->C->nbrows == polka_dec - 1);
  }
}

tbool poly_is_universe_lazy(const poly_t* po)
{
  if (!po->C && !po->F)
    return tbool_false;
  else if (po->C && po->F)
    return ((po->C->nbrows == polka_dec - 1) ? tbool_true : tbool_false);
  else
    return tbool_bottom;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Does a polyhedron satisfy a constraint ?} */
/* %------------------------------------------------------------------------ */

/* Relation between a polyhedron and a constraint. See the function
   \verb-frames_versus_constraint- for precise description. */

tbool poly_versus_constraint(const poly_t* po, const pkint_t* tab)
{
  if (!po->C && !po->F) /* if empty */
    return tbool_top;
  if (!po->F) poly_minimize(po);
  if (!po->F)  /* if empty */
    return tbool_top;
  else
    return frames_versus_constraint(po->F,tab);
}

/* %------------------------------------------------------------------------ */
/* \subsection{Inclusion test of a frame in a polyhedron} */
/* %------------------------------------------------------------------------ */

/* The function says if the frame is included in the polyhedron. */

bool poly_is_generator_included_in(const pkint_t* tab, const poly_t* po)
{
  if (!po->C && !po->F)
    return false;
  if (!po->C) poly_minimize(po);
  if (vector_is_null_strict(tab,po->C->nbcolumns)){
    fprintf(stderr,"poly: poly_is_generator_included_in was given an invalid (null) generator\n");
    exit(1);
  }
  return is_frame_included_in_constraints(tab,po->C);
}

/* %------------------------------------------------------------------------ */
/* \subsection{Inclusion test of polyhedra} */
/* %------------------------------------------------------------------------ */

/* This test requires frames of \verb-pa- and constraints of \verb-pb-. The
   result is true if and only if all frames of \verb-pa- verify the
   constraints of \verb-pb-. We do not require minimality. */

bool poly_is_included_in(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"is_included_in");
  if (!pa->C && !pa->F) /* pa is empty */
    return true;
  if (!pa->F) poly_minimize(pa);
  if (!pa->F) /* pa is empty */
    return true;
  if (!pb->C && !pb->F) /* pb is empty */
    return false;
  if (!pb->C) poly_minimize(pb);

  /* if both are mininmal, check the dimensions */
  if (pa->C && pa->F && pb->C && pb->F
      && (pa->nbeq < pb->nbeq || pa->nbline > pb->nbline))
    {
      return false;
    }
  else {
    /* does the frames of pa satisfy constraints of pb ? */
    int i;
    bool res = true;
    for (i=0; i<pb->C->nbrows; i++){
      tbool tres = frames_versus_constraint(pa->F,pb->C->p[i]);
      if (tres==tbool_false || tres==tbool_bottom){
	res = false;
	break;
      }
    }
    return res;
  }
}

/* %------------------------------------------------------------------------ */
/* \subsection{Equality test} */
/* %------------------------------------------------------------------------ */

bool poly_is_equal(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"is_equal");
  poly_minimize(pa); poly_minimize(pb);
  if (pa->nbeq == pb->nbeq && pa->nbline == pb->nbline){
    return (poly_is_included_in(pa,pb) && poly_is_included_in(pb,pa));
  }
  else
    return false;
}


/* %======================================================================== */
/* \section{Intersection \& Convex Hull} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Intersection} */
/* %------------------------------------------------------------------------ */

poly_t* poly_intersection_array(const poly_t* const *po, const int size)
{
  /* First, special cases */
  if (size==0){
    fflush(stdout);
    fprintf(stderr,
	    "poly_intersection_array: empty array\n");
    exit(-1);
  }
  if (size <= 2){
    if (size==2)
      return poly_intersection(po[0],po[1]);
    else
      return poly_copy(po[0]);
  }
  /* General case */
  else {
    poly_t* npoly;
    matrix_t* mat;
    int dim;
    int nbrows;
    int i,j;

    /* Checks dimensions and minimizes polyhedra, exits if one is empty. */
    /* Counts the number of constraints. */
    /* In the same time find the polyhedron */
    /* to which we will add the constraints of the others (index {\tt j}) */

    dim = po[0]->dim;
    nbrows = 0; /* The number of constraints */
    j = 0; /* The selected start polyhedron */
    for (i=0; i<size; i++){
      if (po[i]->dim != dim){
	fflush(stdout);
	fprintf(stderr,
		"poly_intersection_array: incompatible dimensions\n");
	exit(-1);
      }
      poly_minimize(po[i]);
      if (poly_is_empty(po[i]))
	return poly_empty(dim);
      nbrows += po[i]->C->nbrows;
      if (po[i]->nbeq > po[j]->nbeq ||
	  (po[i]->nbeq == po[j]->nbeq && po[i]->C->nbrows > po[j]->C->nbrows))
	j=i;
    }
    /* Add the other polyehdra to the polyhedra of index {\tt j}. */
    obtain_sorted_C_with_satC(po[j]);
    nbrows -= po[j]->C->nbrows;
    mat = matrix_alloc(nbrows, polka_dec + dim, true);
    mat->nbrows = 0;
    for (i=0; i<size; i++){
      if (i != j){
	obtain_sorted_C(po[i]);
	matrix_add_rows_sort(mat, po[i]->C);
      }
    }
    npoly = _poly_alloc(dim);
    cherni_add_and_minimize(true,
			    po[j]->C, po[j]->F, po[j]->satC, po[j]->nbline,
			    mat,
			    &npoly->C,&npoly->F,&npoly->satF,
			    &npoly->nbeq,&npoly->nbline);
    matrix_free(mat);
    assert(check(npoly));
    return npoly;
  }
}

poly_t* poly_intersection(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"intersection");
  if (poly_is_empty(pa) || poly_is_universe(pb))
    return poly_copy(pa);
  else if (poly_is_empty(pb) || poly_is_universe(pa))
    return poly_copy(pb);
  else {
    poly_t* npoly = _poly_alloc(pa->dim);
    assert(pa->C && pa->F && pb->C && pb->F); /* because of the tests before */
    if ( pa->nbeq > pb->nbeq
	 || (pa->nbeq == pb->nbeq && pa->C->nbrows >= pb->C->nbrows) ){
      obtain_sorted_C_with_satC(pa);
      obtain_sorted_C(pb);
      cherni_add_and_minimize(true,
			  pa->C, pa->F, pa->satC, pa->nbline,
			  pb->C,
			  &npoly->C,&npoly->F,&npoly->satF,
			  &npoly->nbeq,&npoly->nbline);
    } else {
      obtain_sorted_C_with_satC(pb);
      obtain_sorted_C(pa);
      cherni_add_and_minimize(true,
			  pb->C,pb->F,pb->satC, pb->nbline,
			  pa->C,
			  &npoly->C,&npoly->F,&npoly->satF,
			  &npoly->nbeq,&npoly->nbline);
    }
    assert(check(npoly));
    return npoly;
  }
}

poly_t* poly_add_constraints(const poly_t* po, matrix_t* mat)
{
  poly_t* npoly;

  if (po->dim != mat->nbcolumns-polka_dec) {
    fprintf(stderr,
	    "poly: incompatible dimensions of polyhedron and constraints in add_constraints\n");
    exit(-1);
  }
  npoly = _poly_alloc(po->dim);
  poly_minimize(po);
  if (!poly_is_empty(po)){
    obtain_sorted_C_with_satC(po);
    if (!matrix_is_sorted(mat)) matrix_sort_rows(mat);
    cherni_add_and_minimize(true,
			po->C,po->F,po->satC, po->nbline,
			mat,
			&npoly->C,&npoly->F,&npoly->satF,
			&npoly->nbeq,&npoly->nbline);
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_constraint(const poly_t* po, const pkint_t* tab)
{
   pk_poly_matspecial->p[0] = (pkint_t*)tab;
   pk_poly_matspecial->nbcolumns = polka_dec + po->dim;
   return poly_add_constraints(po,pk_poly_matspecial);
}

poly_t* poly_intersection_array_lazy(const poly_t* const *po, const int size)
{
  /* First, special cases */
  if (size==0){
    fflush(stdout);
    fprintf(stderr,
	    "poly_intersection_array: empty array\n");
    exit(-1);
  }
  if (size <= 2){
    if (size==2)
      return poly_intersection_lazy(po[0],po[1]);
    else
      return poly_copy(po[0]);
  }
  /* General case */
  else {
    matrix_t* mat;
    int dim;
    int nbrows;
    int i;

    /* Checks dimensions, exits if one is empty for sure.
       dim = po[0]->dim; */
    dim = po[0]->dim;
    for (i=0; i<size; i++){
      if (po[i]->dim != dim){
	fflush(stdout);
	fprintf(stderr,
		"poly_intersection_array: incompatible dimensions\n");
	exit(-1);
      }
      if (!(po[i]->C || po[i]->F)) return poly_empty(dim);
    }
    /* Get constraint matrices if necessary,
       Counts the number of constraints. */
    nbrows = 0;
    for (i=0; i<size; i++){
      if (!(po[i]->C)) poly_minimize(po[i]);
      nbrows += po[i]->C->nbrows;
    }
    /* Generate the result */
    mat = matrix_alloc(nbrows, polka_dec + dim, true);
    mat->nbrows = 0;
    for (i=0; i<size; i++){
      obtain_sorted_C(po[i]);
      matrix_add_rows_sort(mat, po[i]->C);
    }
    return poly_of_constraints(mat);
  }
}

poly_t* poly_intersection_lazy(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"intersection");

  if (poly_is_empty_lazy(pa)==tbool_true || poly_is_empty_lazy(pb)==tbool_true)
    return poly_empty(pa->dim);
  else {
    if (!pa->C) poly_minimize(pa);
    if (!pb->C) poly_minimize(pb);
    if (poly_is_universe_lazy(pb)==tbool_true)
      return poly_copy(pa);
    else if (poly_is_universe_lazy(pa)==tbool_true)
      return poly_copy(pb);
    else {
      obtain_sorted_C(pa); obtain_sorted_C(pb);
      return poly_of_constraints(matrix_merge_sort(pa->C, pb->C));
    }
  }
}

poly_t* poly_add_constraints_lazy(const poly_t* po, matrix_t* mat)
{
  poly_t* npoly;

  if (po->dim != mat->nbcolumns-polka_dec) {
    fprintf(stderr,
	    "poly: incompatible dimensions of polyhedra and constraints in add_constraints\n");
    exit(-1);
    }
  npoly = _poly_alloc(po->dim); /* = empty polyedron */
  if (po->C || po->F){
    if (!po->C) poly_minimize(po);
    obtain_sorted_C(po);
    npoly->C = matrix_merge_sort(po->C, mat);
  }
  return npoly;
}

poly_t* poly_add_constraint_lazy(const poly_t* po, const pkint_t* tab)
{
   pk_poly_matspecial->p[0] = (pkint_t*)tab;
   pk_poly_matspecial->nbcolumns = polka_dec + po->dim;
   return poly_add_constraints_lazy(po,pk_poly_matspecial);
}

/* %------------------------------------------------------------------------ */
/* \subsection{Convex Hull} */
/* %------------------------------------------------------------------------ */

poly_t* poly_convex_hull_array(const poly_t* const *po, const int size)
{
  /* First, special cases */
  if (size==0){
    fflush(stdout);
    fprintf(stderr,
	    "poly_intersection_array: empty array\n");
    exit(-1);
  }
  if (size <= 2){
    if (size==2)
      return poly_convex_hull(po[0],po[1]);
    else
      return poly_copy(po[0]);
  }
  /* General case */
  else {
    poly_t* npoly;
    matrix_t* mat;
    int dim;
    int nbrows;
    int i,j;

    /* Checks dimensions and minimizes polyhedra, exits if one is universe. */
    /* Counts the number of rays. */
    /* In the same time find the polyhedron */
    /* to which we will add the constraints of the others (put in index {\tt j}. */
    dim = po[0]->dim;
    nbrows = 0; /* The number of constraints */
    j = (-1);   /* The selected polyhedron */
    for (i=0; i<size; i++){
      if (po[i]->dim != dim){
	fflush(stdout);
	fprintf(stderr,
		"poly_intersection_array: incompatible dimensions\n");
	exit(-1);
      }
      poly_minimize(po[i]);
      if (poly_is_universe(po[i]))
	return poly_universe(dim);
      else if (po[i]->F) {
	nbrows += po[i]->F->nbrows;
	if (j<0 || po[i]->nbline > po[j]->nbline ||
	    (po[i]->nbline == po[j]->nbline && po[i]->F->nbrows > po[j]->F->nbrows))
	  j=i;
      }
    }
    if (j<0){ /* All polyhedra are empty */
      return poly_empty(dim);
    }
    else if (po[j]->F->nbrows == nbrows){ /* Only one is non empty */
      return poly_copy(po[j]);
    }
    else { /* Normal case */
      obtain_sorted_F_with_satF(po[j]);
      nbrows -= po[j]->F->nbrows;
      mat = matrix_alloc(nbrows, polka_dec + dim, true);
      mat->nbrows = 0;
      for (i=0; i<size; i++){
	if (i != j && po[i]->F){
	  obtain_sorted_F(po[i]);
	  matrix_add_rows_sort(mat, po[i]->F);
	}
      }
      npoly = _poly_alloc(dim);
      cherni_add_and_minimize(false,
			      po[j]->F, po[j]->C, po[j]->satF, po[j]->nbeq,
			      mat,
			      &npoly->F,&npoly->C,&npoly->satC,
			      &npoly->nbline,&npoly->nbeq);
      matrix_free(mat);
      assert(check(npoly));
      return npoly;
    }
  }
}

poly_t* poly_convex_hull(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"convex_hull");
  poly_minimize(pa); poly_minimize(pb);
  if (poly_is_empty(pa) || poly_is_universe(pb))
    return poly_copy(pb);
  else if (poly_is_empty(pb) || poly_is_universe(pa))
    return poly_copy(pa);
  else {
    poly_t* npoly = _poly_alloc(pa->dim);
    assert(pa->C && pa->F && pb->C && pb->F); /* because of the tests before */
    if (pa->nbline > pb->nbline
	|| (pa->nbline == pb->nbline && pa->F->nbrows >= pb->F->nbrows)) {
      obtain_sorted_F_with_satF(pa);
      obtain_sorted_F(pb);
      cherni_add_and_minimize(false,
			  pa->F,pa->C,pa->satF, pa->nbeq,
			  pb->F,
			  &npoly->F,&npoly->C,&npoly->satC,
			  &npoly->nbline,&npoly->nbeq);
    }
    else {
      obtain_sorted_F_with_satF(pb);
      obtain_sorted_F(pa);
      cherni_add_and_minimize(false,
			  pb->F,pb->C,pb->satF, pb->nbeq,
			  pa->F,
			  &npoly->F,&npoly->C,&npoly->satC,
			  &npoly->nbline,&npoly->nbeq);
    }
    assert(check(npoly));
    return npoly;
  }
}

poly_t* poly_add_frames(const poly_t* po, matrix_t* mat)
{
  poly_t* npoly;

  if (po->dim != mat->nbcolumns-polka_dec) {
    fprintf(stderr,
	    "poly: incompatible dimensions of polyhedron and frames in add_frames\npo->dim=%d, mat->nbcolumns=%d\n",po->dim,mat->nbcolumns);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim);
  if (! poly_is_empty(po)){
    poly_minimize(po);
    obtain_sorted_F_with_satF(po);
    if (!matrix_is_sorted(mat)) matrix_sort_rows(mat);
    cherni_add_and_minimize(false,
			    po->F,po->C,po->satF, po->nbeq,
			    mat,
			    &npoly->F,&npoly->C,&npoly->satC,
			    &npoly->nbline,&npoly->nbeq);
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_frame(const poly_t* po, const pkint_t* tab)
{
  pk_poly_matspecial->p[0] = (pkint_t*)tab;
  pk_poly_matspecial->nbcolumns = polka_dec + po->dim;
  return poly_add_frames(po,pk_poly_matspecial);
}

poly_t* poly_convex_hull_array_lazy(const poly_t* const *po, const int size)
{
  /* First, special cases */
  if (size==0){
    fflush(stdout);
    fprintf(stderr,
	    "poly_intersection_array_lazy: empty array\n");
    exit(-1);
  }
  if (size <= 2){
    if (size==2)
      return poly_convex_hull_lazy(po[0],po[1]);
    else
      return poly_copy(po[0]);
  }
  /* General case */
  else {
    matrix_t* mat;
    int dim;
    int nbrows;
    int i;

    /* Checks dimensions.
       Counts the number of frames. */
    dim = po[0]->dim;
    nbrows = 0; /* The number of constraints */
    for (i=0; i<size; i++){
      if (po[i]->dim != dim){
	fflush(stdout);
	fprintf(stderr,
		"poly_convex_hull_array_lazy: incompatible dimensions\n");
	exit(-1);
      }
      if (!po[i]->F) poly_minimize(po[i]);
      if (poly_is_universe_lazy(po[i]))
	return poly_universe(dim);
      else if (po[i]->F) {
	nbrows += po[i]->F->nbrows;
      }
    }
    mat = matrix_alloc(nbrows, polka_dec + dim, true);
    mat->nbrows = 0;
    for (i=0; i<size; i++){
      if (po[i]->F){
	obtain_sorted_F(po[i]);
	matrix_add_rows_sort(mat, po[i]->F);
      }
    }
    return poly_of_frames(mat);
  }
}

poly_t* poly_convex_hull_lazy(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"convex_hull");
  if (poly_is_empty_lazy(pa)==tbool_true || poly_is_universe_lazy(pb)==tbool_true)
    return poly_copy(pb);
  if (poly_is_empty_lazy(pb)==tbool_true || poly_is_universe_lazy(pa)==tbool_true)
    return poly_copy(pa);
  else {
    if (!pa->F){
      poly_minimize(pa);
      if (!pa->F) /* if pa empty */
	return poly_copy(pb);
      else if (poly_is_universe(pa)){
	return poly_copy(pa);
      }
    }
    if (!pb->F){
      poly_minimize(pb);
      if (!pb->F) /* if pb empty */
	return poly_copy(pa);
      else if (poly_is_universe(pb)){
	return poly_copy(pb);
      }
    }
    obtain_sorted_F(pa); obtain_sorted_F(pb);
    return poly_of_frames(matrix_merge_sort(pa->F,pb->F));
  }
}

poly_t* poly_add_frames_lazy(const poly_t* po, matrix_t* mat)
{
  if (po->dim != mat->nbcolumns-polka_dec) {
    fprintf(stderr,
	    "poly: incompatible dimensions of polyhedra and frames in add_frames\n");
    exit(-1);
    }
  if (!po->F) poly_minimize(po);
  if (!po->F){
    return _poly_alloc(po->dim);
  }
  else if (poly_is_universe_lazy(po)==tbool_true){
    return poly_copy(po);
  }
  else {
    obtain_sorted_F(po);
    return poly_of_frames(matrix_merge_sort(po->F,mat));
  }
}

poly_t* poly_add_frame_lazy(const poly_t* po, const pkint_t* tab)
{
   pk_poly_matspecial->p[0] = (pkint_t*)tab;
   pk_poly_matspecial->nbcolumns = polka_dec + po->dim;
   return poly_add_frames_lazy(po,pk_poly_matspecial);
}

/* %======================================================================== */
/* \section{Change of dimension} */
/* %======================================================================== */

/* %------------------------------------------------------------------------ */
/* \subsection{Addition of new dimensions with embeding} */
/* %------------------------------------------------------------------------ */

poly_t* poly_add_dimensions_and_embed(const poly_t* po, int dimsup)
{
  poly_t* npoly;

  if (dimsup<0){
    fprintf(stderr,"Polka: poly.c: poly_add_dimensions_and_embed called with a negative dimsup=%d\n",
	    dimsup);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim + dimsup);
  cherni_add_dimensions(po->C,po->F,&((poly_t*)po)->satC,po->satF,dimsup,
			&npoly->C,&npoly->F,&npoly->satC);
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq;
    npoly->nbline = po->nbline + dimsup;
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_dimensions_and_embed_multi(const poly_t* po,
					    const dimsup_t* tab, int size)
{
  int dimsup;
  poly_t* npoly = _poly_alloc(po->dim);
  cherni_add_dimensions_multi(po->C,po->F,&((poly_t*)po)->satC,po->satF,
			      tab,size,
			      &npoly->C,&npoly->F,&npoly->satC,&dimsup);
  npoly->dim = po->dim + dimsup;
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq;
    npoly->nbline = po->nbline + dimsup;
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_permute_dimensions_and_embed(const poly_t* po,
					      const int dimsup,
					      const int * const permutation)
{
  poly_t* npoly;

  if (dimsup<0){
    fprintf(stderr,"Polka: poly.c: poly_add_permute_dimensions_and_embed called with a negative dimsup=%d\n",
	    dimsup);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim+dimsup);
  cherni_add_permute_dimensions(po->C,po->F,&((poly_t*)po)->satC,po->satF,
				dimsup,permutation,
				&npoly->C,&npoly->F,&npoly->satC);
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq;
    npoly->nbline = po->nbline + dimsup;
  }
  assert(check(npoly));
  return npoly;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Addition of new dimensions with projection} */
/* %------------------------------------------------------------------------ */

poly_t* poly_add_dimensions_and_project(const poly_t* po, int dimsup)
{
  poly_t* npoly;

  if (dimsup<0){
    fprintf(stderr,"Polka: poly.c: poly_add_dimensions_and_project called with a negative dimsup=%d\n",
	    dimsup);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim + dimsup);
  cherni_add_dimensions(po->F,po->C,&((poly_t*)po)->satF,po->satC,dimsup,
			&npoly->F,&npoly->C,&npoly->satF);
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq + dimsup;
    npoly->nbline = po->nbline;
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_dimensions_and_project_multi(const poly_t* po,
					      const dimsup_t* tab, int size)
{
  int dimsup;
  poly_t* npoly = _poly_alloc(po->dim);
  cherni_add_dimensions_multi(po->F,po->C,&((poly_t*)po)->satF,po->satC,
			      tab,size,
			      &npoly->F,&npoly->C,&npoly->satF,&dimsup);
  npoly->dim = po->dim + dimsup;
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq + dimsup;
    npoly->nbline = po->nbline;
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_add_permute_dimensions_and_project(const poly_t* po,
						const int dimsup,
						const int * const permutation)
{
  poly_t* npoly;

  if (dimsup<0){
    fprintf(stderr,"Polka: poly.c: poly_add_change_permute_dimensions_and_project called with a negative dimsup=%d\n",
	    dimsup);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim+dimsup);
  cherni_add_permute_dimensions(po->F,po->C,&((poly_t*)po)->satF,po->satC,
				dimsup,permutation,
				 &npoly->F,&npoly->C,&npoly->satF);
  if (po->C && po->F){ /* si le polyhedre est minimal */
    npoly->nbeq = po->nbeq + dimsup;
    npoly->nbline = po->nbline;
  }
  assert(check(npoly));
  return npoly;
}

/* %------------------------------------------------------------------------ */
/* \subsection{Remove dimensions} */
/* %------------------------------------------------------------------------ */


/* This function projects the polyhedron onto the \verb|dim-dimsup| first
   dimensions and eliminates the last coefficients. Minimality may be lost. */
poly_t* poly_remove_dimensions(const poly_t* po, int dimsup)
{
  poly_t* npoly;
  matrix_t* C;
  matrix_t* F;
  int i,j;
  bool test;

  if (dimsup <= 0 || dimsup>=po->dim){
    fprintf(stderr,
	    "Polka: poly.c: poly_remove_dimensions : inconsistent dimension.\npo->dim=%d,dimsup=%d\n",
	    po->dim,dimsup);
    exit(-1);
  }

  npoly = _poly_alloc(po->dim-dimsup);

  if (!po->C && po->F){
    matrix_t* F = (matrix_t*)po->F;
    F->nbcolumns -= dimsup;
    npoly->F = matrix_copy(F);
    F->nbcolumns += dimsup;
  }
  else if (po->C){
    if (!po->F) poly_minimize(po);
    F = (matrix_t*)po->F;
    /* Now, we know that po is minimized */
    /* Let us check that we have the last lines at the beginning of
       F. In such a case, the polyhedron is already "projected" on the
       final dimensions and we can keep the constraints. */
    if (po->nbline < dimsup){
      test = false;
    }
    else {
      obtain_sorted_F(po);
      test = true;
      for(i=0; i<dimsup; i++){
	for (j=0; j<F->nbcolumns; j++){
	  int sign = pkint_sgn(F->p[i][j]);
	  if ( ( (j==F->nbcolumns-1-i) && !sign ) ||
	       ( (j!=F->nbcolumns-1-i) && sign ) ){
	    test = false;
	    goto exit;
	  }
	}
      }
    }
  exit:
    if (test){
      /* The answser is yes, and F is sorted */
      obtain_satC(po);
      npoly->satC = satmat_alloc(F->nbrows-dimsup, po->satC->nbcolumns);
      npoly->F = _matrix_alloc_int(F->nbrows-dimsup, F->nbcolumns-dimsup,true);
      for (i=0; i<F->nbrows-dimsup; i++){
	for (j=0; j<po->satC->nbcolumns; j++){
	  npoly->satC->p[i][j] = po->satC->p[dimsup+i][j];
	}
	for (j=0; j<po->F->nbcolumns-dimsup; j++){
	  pkint_init_set(npoly->F->p[i][j],F->p[dimsup+i][j]);
	}
      }
      C = (matrix_t*)po->C;
      C->nbcolumns -= dimsup;
      npoly->C = matrix_copy(C);
      C->nbcolumns += dimsup;
      npoly->nbeq = po->nbeq;
      npoly->nbline = po->nbline - dimsup;
    }
    else {
      F->nbcolumns -= dimsup;
      npoly->F = matrix_copy(F);
      F->nbcolumns += dimsup;
    }
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_remove_dimensions_multi(const poly_t* po, const dimsup_t* tab, int size)
{
  int dimsup;
  poly_t* npoly;

  /* Checks the input */
  dimsup = check_remove_dimsup_of_multi(polka_dec+po->dim,tab,size);

  npoly = _poly_alloc(po->dim-dimsup);
  if (po->C || po->F){
    if (!po->F) poly_minimize(po);
    if (po->F){
      npoly->F = matrix_remove_dimensions_multi(po->F,tab,size);
    }
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_permute_remove_dimensions(const poly_t* po,
				       const int dimsup,
				       const int * const permutation)
{
  poly_t* npoly;

  if (dimsup <= 0 || dimsup>=po->dim){
    fprintf(stderr,
	    "Polka: poly.c: poly_permute_remove_dimensions : inconsistent dimension\npo->dim=%d,dimsup=%d\n",
	    po->dim,dimsup);
    exit(-1);
  }

  npoly = _poly_alloc(po->dim-dimsup);
  if (po->C || po->F){
    if (!po->F) poly_minimize(po);
    if (po->F){
      npoly->F = matrix_permute_remove_dimensions(po->F,
						  dimsup,
						  permutation);
    }
  }
  assert(check(npoly));
  return npoly;
}

/* %======================================================================== */
/* \section{Linear transformation} */
/* %======================================================================== */

/* %======================================================================== */
/* \subsection{Transformation of a single variable by a linear expression} */
/* %======================================================================== */

/* In the old version, variables were numbered from \verb|polka_dec|
   up to \verb|polka_dec+dim-1|. In the new version, they are numbered
   from \verb|0| up to \verb|polka_dim-1|. */

/* %------------------------------------------------------------------------ */
/* \subsubsection{Old version} */
/* %------------------------------------------------------------------------ */

/*
These functions distinguishes inversible affectations and substitutions from
not inversible ones. This allows to work with any representation in the first
case. $tab = [d,b,a_0,\ldots,a_{dim-1}]$ represents the expression
$\displaystyle{\frac{\sum_{i=0}^{dim-1} a_i x_i + b}{d}}$.  Normal variable
are numeroted from \verb|polka_dec| up to \verb|polka_dec+dim-1|.
*/

poly_t* poly_assign_variable_old(const poly_t* po, int var, const pkint_t* const tab)
{
  int i;
  poly_t* npoly;
  int sgn;

  if (var<polka_dec || var >= polka_dec + po->dim){
    fprintf(stderr,
	    "poly: var=%d not in [polka_dec=%d,polka_dec+po->dim-1=%d] in poly_assign_variable_old\n",var,polka_dec,polka_dec+po->dim-1);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim);
  if (po->C || po->F){
    sgn = pkint_sgn(tab[var]);
    if (sgn) { /* Expression is invertible. */
      if (po->F)
	npoly->F = matrix_assign_variable(po->F, var, tab);
      if (po->C){
	/* Invert the expression in {\tt tab}. */
	if (sgn>0){
	  pkint_set(pk_poly_pkintp[0], tab[var]);
	  pkint_set(pk_poly_pkintp[var], tab[0]);
	  for (i=1; i<po->C->nbcolumns; i++){
	    if (i!=var)
	      pkint_neg(pk_poly_pkintp[i],tab[i]);
	  }
	} else {
	  pkint_neg(pk_poly_pkintp[0], tab[var]);
	  pkint_neg(pk_poly_pkintp[var], tab[0]);
	  for (i=1; i<po->C->nbcolumns; i++){
	    if (i!=var)
	      pkint_set(pk_poly_pkintp[i],tab[i]);
	  }
	}
	/* Perform susbtitution on constraints. */
	npoly->C = matrix_substitute_variable(po->C, var, pk_poly_pkintp);
	/* Saturation matrices are the same. */
	if (po->satC) npoly->satC = satmat_copy(po->satC);
	if (po->satF) npoly->satF = satmat_copy(po->satF);
	npoly->nbeq = po->nbeq;
	npoly->nbline = po->nbline;
      }
    } else { /* The expression is not invertible. */
      if (!po->F) poly_minimize(po);
      if (po->F)
	npoly->F = matrix_assign_variable(po->F, var, tab);
    }
  }
  assert(check(npoly));
  return npoly;
}

poly_t* poly_substitute_variable_old(const poly_t* po, int var, const pkint_t* const tab)
{
  int i;
  poly_t* npoly;
  int sgn;

  if (var<polka_dec || var >= polka_dec + po->dim){
    fprintf(stderr,
	    "poly: var=%d not in [polka_dec=%d,polka_dec+po->dim-1=%d] in poly_substitute_variable_old\n",var,polka_dec,polka_dec+po->dim-1);
    exit(-1);
  }
  npoly = _poly_alloc(po->dim);
  if (po->C || po->F){
    sgn = pkint_sgn(tab[var]);
    if (sgn) {  /* Expression is invertible. */
      if (po->C)
	npoly->C = matrix_substitute_variable(po->C, var, tab);
      if (po->F){
	/* Invert the expression in {\tt tab}. */
	if (sgn>0){
	  pkint_set(pk_poly_pkintp[0], tab[var]);
	  pkint_set(pk_poly_pkintp[var], tab[0]);
	  for (i=1; i<po->C->nbcolumns; i++){
	    if (i!=var)
	      pkint_neg(pk_poly_pkintp[i],tab[i]);
	  }
	} else {
	  pkint_neg(pk_poly_pkintp[0], tab[var]);
	  pkint_neg(pk_poly_pkintp[var], tab[0]);
	  for (i=1; i<po->C->nbcolumns; i++){
	    if (i!=var)
	      pkint_set(pk_poly_pkintp[i],tab[i]);
	  }
	}
	/* Perform assginement of generators. */
	npoly->F = matrix_assign_variable(po->F, var, pk_poly_pkintp);
	/* Saturation matrices are the same. */
	if (po->satC) npoly->satC = satmat_copy(po->satC);
	if (po->satF) npoly->satF = satmat_copy(po->satF);
	npoly->nbeq = po->nbeq;
	npoly->nbline = po->nbline;
      }
    }
    else { /* The expression is not invertible. */
      if (!po->C) poly_minimize(po);
      npoly->C = matrix_substitute_variable(po->C, var, tab);
    }
  }
  assert(check(npoly));
  return npoly;
}

/* %------------------------------------------------------------------------ */
/* \subsubsection{New version} */
/* %------------------------------------------------------------------------ */

/*
These functions distinguishes inversible affectations and substitutions from
not inversible ones. This allows to work with any representation in the first
case. $tab = [d,b,a_0,\ldots,a_{dim-1}]$ represents the expression
$\displaystyle{\frac{\sum_{i=0}^{dim-1} a_i x_i + b}{d}}$.  Normal variable
are numeroted from \verb|0| up to \verb|dim-1|.
*/

poly_t* poly_assign_variable(const poly_t* po, int var, const pkint_t* const tab)
{
  if (var<0 || var >= po->dim){
    fprintf(stderr,
	    "poly: var=%d not in [0,po->dim-1=%d] in poly_assign_variable\n",var,po->dim-1);
    exit(-1);
  }
  return poly_assign_variable_old(po,polka_dec+var,tab);
}

poly_t* poly_substitute_variable(const poly_t* po, int var, const pkint_t* const tab)
{
  if (var<0 || var >= po->dim){
    fprintf(stderr,
	    "poly: var=%d not in [0,po->dim-1=%d] in poly_substitute_variable\n",var,po->dim-1);
    exit(-1);
  }
  return poly_substitute_variable_old(po,polka_dec+var,tab);
}

/* %======================================================================== */
/* \subsection{Simultaneous transformation of a set of variable by a set of linear expression} */
/* %======================================================================== */

poly_t* poly_assign_variables(const poly_t* po, const equation_t * eqn, const int size)
{
  int i;
  poly_t* npoly = _poly_alloc(po->dim);

  for (i=0; i<size; i++){
    if (eqn[i].var < 0 || eqn[i].var >= po->dim){
      fprintf(stderr,
	      "Polka: poly.c: poly_assign_variables: invalid value for eqn[%d].var=%d\nnot in [0,po->dim-1=%d]",
	      i,eqn[i].var,po->dim-1);
      exit(-1);
    }
  }
  translate_equations((equation_t*)eqn,size,polka_dec);
  if (po->C || po->F){
    if (!po->F) poly_minimize(po);
    if (po->F)
      npoly->F = matrix_assign_variables(po->F, eqn, size);
  }
  translate_equations((equation_t*)eqn,size,- (polka_dec));
  assert(check(npoly));
  return npoly;
}


poly_t* poly_substitute_variables(const poly_t* po, const equation_t * eqn, const int size)
{
  int i;
  poly_t* npoly = _poly_alloc(po->dim);

  for (i=0; i<size; i++){
    if (eqn[i].var < 0 || eqn[i].var >= po->dim){
      fprintf(stderr,
	      "Polka: poly.c: poly_assign_variables: invalid value for eqn[%d].var=%d\nnot in [0,po->dim-1=%d]",
	      i,eqn[i].var,po->dim-1);
      exit(-1);
    }
  }
  translate_equations((equation_t*)eqn,size,polka_dec);
  if (po->C || po->F){
    if (!po->C) poly_minimize(po);
    npoly->C = matrix_substitute_variables(po->C, eqn, size);
  }
  translate_equations((equation_t*)eqn,size,-(polka_dec));
  assert(check(npoly));
  return npoly;
}


/* %======================================================================== */
/* /\section{Widening} */
/* %======================================================================== */

/* This function defines the standard widening operator.  The resulting
   polyhedron has no frame matrix, unless \verb-pa- is empty. */
poly_t* poly_widening(const poly_t* pa, const poly_t* pb)
{
  check_dims(pa,pb,"widening");
  poly_minimize(pa); poly_minimize(pb);
  if (!pa->C && !pa->F) /* pa is empty */
    return poly_copy(pb);
  else {
    int sat_nbcols;
    int nbrows,i,j,index;
    satmat_t* sat;
    poly_t* npoly = _poly_alloc(pa->dim);

    /* copy saturation pa->satF, and sort it */
    obtain_satF(pa);
    sat = satmat_copy(pa->satF);
    satmat_sort_rows(sat);
    sat_nbcols = sat->nbcolumns;

    npoly->C = matrix_alloc(polka_dec-1+pb->C->nbrows, pb->C->nbcolumns, false);
    if (polka_strict){
      /* constraint $\epsilon \geq 0$ */
      pkint_set_ui(npoly->C->p[0][0],1);
      pkint_set_ui(npoly->C->p[0][polka_eps],1);
      /* constraint $\xi - \epsilon \geq 0$ */
      pkint_set_ui(npoly->C->p[1][0],1);
      pkint_set_ui(npoly->C->p[1][polka_cst],1);
      pkint_set_si(npoly->C->p[2][polka_eps],(-1));
      nbrows = 2;
    }
    else {
      /* constraint $\xi \geq 0$ */
      pkint_set_ui(npoly->C->p[0][0],1);
      pkint_set_ui(npoly->C->p[0][polka_cst],1);
      nbrows = 1;
    }
    /* adding constraints of pb mutually redundant with some of pa, except if
     it is mutually redundant with the positivity constraint of pa only. */
    for (i=0; i<pb->C->nbrows; i++){
      bitstring_clear(pk_poly_bitstringp,sat_nbcols);
      cherni_buildsatline(pa->F, pb->C->p[i],pk_poly_bitstringp);
      index = satmat_index_in_sorted_rows(pk_poly_bitstringp,sat);
      if ( index>=0 &&
	  (!polka_widening_affine ||
	   !vector_is_positivity_constraint(pa->C->p[index],pa->C->nbcolumns)) ){
	/* Belongs to saturation matrix, but does not correspond to the
	   positivity constraint. */
	for (j=0; j<pb->C->nbcolumns; j++)
	  pkint_set(npoly->C->p[nbrows][j],pb->C->p[i][j]);
	nbrows++;
      }
    }
    satmat_free(sat);
    npoly->C->nbrows = nbrows;
    assert(check(npoly));
    return npoly;
  }
}

/* This second one implements a version parametrized by a set of constraints:
when a constraint of this set is verified by both polyhedra, it is kept in
the result. */
poly_t* poly_limited_widening(const poly_t* pa, const poly_t* pb, matrix_t* mat)
{
  check_dims(pa,pb,"widening");
  poly_minimize(pa); poly_minimize(pb);
  if (!pa->C && !pa->F) /* pa is empty */
    return poly_copy(pb);
  else {
    int i,nbrows;
    const int mat_nbrows = mat->nbrows;
    poly_t* npoly = poly_widening(pa,pb);

    /* We assume here that npoly->F=0, as well as the saturation matrices */
    nbrows = 0;
    for (i=0; i<mat_nbrows; i++){
      switch (frames_versus_constraint(pb->F,mat->p[i])) {
      case tbool_true:
      case tbool_top:
	/* if the constraint is satisfied by p2, put it in first places */
	matrix_exch_rows(mat,nbrows,i);
	nbrows++;
	break;
      default:
	break;
      }
    }
    if (nbrows>0){
      mat->nbrows = nbrows;
      mat->_sorted = false;
      if (npoly->C->nbrows + nbrows <= npoly->C->_maxrows){
	/* we do not need to allocate a new matrix,
	   see widening function */
	matrix_add_rows_sort(npoly->C,mat);
      }
      else {
	matrix_t* nC = matrix_merge_sort(npoly->C,mat);
	matrix_free(npoly->C);
	npoly->C = nC;
      }
      mat->nbrows = mat_nbrows;
      mat->_sorted = false;
    }
    assert(check(npoly));
    return npoly;
  }
}

/* %======================================================================== */
/* \section{Closure operation} */
/* %======================================================================== */

/* Closure operation.

Returns a new polyhedron which is the closure (in the topological sense) of
the argument polyhedron. In other words, returns the smallest closed
polyhedron (ie, with loose constraints only) containing the argument.
Strict and non-strict versions.
*/


static bool poly_closure_is_loose(const matrix_t* C)
{
  int i;
  bool loose = true;
  for (i=0; i<C->nbrows; i++){
    int strict = pkint_sgn(C->p[i][polka_eps]);
    if (strict<0){
      if (! vector_is_positivity_constraint(C->p[i],C->nbcolumns)){
	loose = false;
	break;
      }
    }
  }
  return loose;
}

static matrix_t* poly_closure_loosen(const matrix_t* C)
{
  int i,j;

  matrix_t* nC = _matrix_alloc_int(C->nbrows+1, C->nbcolumns, false);
  // Loosen strict constraints
  for (i=0; i<C->nbrows; i++){
    pkint_init_set(nC->p[i][0], C->p[i][0]);
    pkint_init_set(nC->p[i][polka_cst], C->p[i][polka_cst]);
    if (pkint_sgn(C->p[i][polka_eps])<0)
      pkint_init_set_ui(nC->p[i][polka_eps],0);
    else
      pkint_init_set(nC->p[i][polka_eps], C->p[i][polka_eps]);

    for (j=polka_eps+1; j<C->nbcolumns; j++){
      pkint_init_set(nC->p[i][j], C->p[i][j]);
    }
    if (pkint_sgn(C->p[i][polka_eps])<0){
      vector_normalize(nC->p[i],C->nbcolumns);
    }
  }
  // Add again positivity constraint to bound epsilon
  pkint_init_set_si(nC->p[C->nbrows][0], 1);
  pkint_init_set_si(nC->p[C->nbrows][polka_cst], 1);
  pkint_init_set_si(nC->p[C->nbrows][polka_eps], -1);
  for (j=polka_eps+1; j<C->nbcolumns; j++){
    pkint_init_set_ui(nC->p[C->nbrows][j], 0);
  }
  return nC;
}



poly_t* poly_closure(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;

  poly_minimize(po);
  if (!polka_strict || poly_is_empty(po)){ // po cannot contain strict constraints
    return poly_copy(po);
  }

  // Obtain C
  matrix_t* C = po->C;

  // Does C contains only loose constraints ?
  bool loose = poly_closure_is_loose(C);

  if (loose){ // po does not contain strict constraints
    return poly_copy(po);
  }
  else {
    matrix_t* nC = poly_closure_loosen(C);
    poly_t* npo = _poly_alloc(po->dim);
    npo->C = nC;
    poly_minimize(npo);
    return npo;
  }
}

/* Closure operation, lazy version. */

poly_t* poly_closure_lazy(const poly_t* poly)
{
  poly_t* po = (poly_t*)poly;

  if (!polka_strict){ // po cannot contain strict constraints
    return poly_copy(po);
  }

  // Obtain C
  if (!po->C) poly_minimize(po);
  matrix_t* C = po->C;

  // Does C contains only loose constraints ?
  bool loose = poly_closure_is_loose(C);

  if (loose){ // po does not contain strict constraints
    return poly_copy(po);
  }
  else {
    matrix_t* nC = poly_closure_loosen(C);
    poly_t* npo = _poly_alloc(po->dim);
    npo->C = nC;
    return npo;
  }
}
