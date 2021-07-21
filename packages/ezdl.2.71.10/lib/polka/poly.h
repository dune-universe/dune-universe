/*% $Id: poly.h,v 1.3 2003/12/12 14:07:19 bjeannet Exp $ */

/* This header file define operations on polyhedra. */

/* The invariant of the representation of a polyhedron is the following:
if the polyhedron is empty, then \verb-C=F=satC=satF=0-. Else, we have 
\verb-(C || F) && (satC || satF || !(C && F))-. This means that a
polyhedron has a minimal representation minimal if and only if \verb-C && F- 
if and only if \verb-satC || satF-. */

#ifndef __POLKA_POLY_H__
#define __POLKA_POLY_H__

#include "config.h"
#include "polka.h"
#include "pkint.h"
#include "vector.h"
#include "satmat.h"
#include "matrix.h"

typedef struct poly_t {
  /* private data: do not use directly ! */
  matrix_t* C;
  matrix_t* F;
  satmat_t* satC;
  satmat_t* satF;
  int dim;
  int nbeq;
  int nbline;
} poly_t;

/* Basic Operations */
poly_t* _poly_alloc(int dim); /* do not use ! */
void    poly_free(poly_t* po);
poly_t* poly_copy(const poly_t* po);
void    poly_print(const poly_t* po);
void    poly_minimize(const poly_t* po);
void    poly_canonicalize(const poly_t* po);

/* Basic constructor */
poly_t* poly_empty(int pdim);
poly_t* poly_universe(int pdim);
poly_t* poly_of_constraints(matrix_t* mat);
poly_t* poly_of_frames(matrix_t* mat);

/* Access Functions */
const matrix_t* poly_constraints(const poly_t* po);
const matrix_t* poly_frames(const poly_t* po);
const satmat_t* poly_satC(const poly_t* po);
const satmat_t* poly_satF(const poly_t* po);
int poly_dimension(const poly_t* po);
int poly_nbequations(const poly_t* po);
int poly_nblines(const poly_t* po);
int poly_nbconstraints(const poly_t* po);
int poly_nbframes(const poly_t* po);

/* Predicates */
bool poly_is_minimal(const poly_t* po);
bool poly_is_empty(const poly_t* po);
bool poly_is_universe(const poly_t* po);
tbool poly_is_empty_lazy(const poly_t* po);
tbool poly_is_universe_lazy(const poly_t* po);
tbool poly_versus_constraint(const poly_t* po,const pkint_t* tab);
bool poly_is_generator_included_in(const pkint_t* tab, const poly_t* po);
bool poly_is_included_in(const poly_t* pa, const poly_t* pb);
bool poly_is_equal(const poly_t* pa, const poly_t* pb);

/* Change of dimension */
poly_t* poly_add_dimensions_and_embed(const poly_t* po, int dimsup);
poly_t* poly_add_dimensions_and_project(const poly_t* po, int dimsup);
poly_t* poly_remove_dimensions(const poly_t* po, int dimsup);

poly_t* poly_add_dimensions_and_embed_multi(const poly_t* po, 
					    const dimsup_t* tab, int size);
poly_t* poly_add_dimensions_and_project_multi(const poly_t* po,
					      const dimsup_t* tab, int size);
poly_t* poly_remove_dimensions_multi(const poly_t* po,
				     const dimsup_t* tab, int size);

poly_t* poly_add_permute_dimensions_and_embed(const poly_t* po,
					      int dimsup,
					      const int* permutation);
poly_t* poly_add_permute_dimensions_and_project(const poly_t* po,
						int dimsup,
						const int* permutation);
poly_t* poly_permute_remove_dimensions(const poly_t* po,
				       int dimsup,
				       const int* permutation);

/* Intersection and Convex Hull */
poly_t*  poly_intersection_array(const poly_t* const *po, int size);
poly_t*  poly_intersection(const poly_t* pa, const poly_t* pb);
poly_t*  poly_add_constraints(const poly_t* po, matrix_t* mat);
poly_t*  poly_add_constraint(const poly_t* po, const pkint_t* tab);

poly_t*  poly_convex_hull_array(const poly_t* const *po, int size);
poly_t* poly_convex_hull(const poly_t* pa, const poly_t* pb);
poly_t* poly_add_frames(const poly_t* po, matrix_t* mat);
poly_t* poly_add_frame(const poly_t* po, const pkint_t* tab);

poly_t*  poly_intersection_array_lazy(const poly_t* const *po, int size);
poly_t* poly_intersection_lazy(const poly_t* pa, const poly_t* pb);
poly_t* poly_add_constraints_lazy(const poly_t* po, matrix_t* mat);
poly_t* poly_add_constraint_lazy(const poly_t* po, const pkint_t* tab);

poly_t*  poly_convex_hull_array_lazy(const poly_t* const *po, int size);
poly_t* poly_convex_hull_lazy(const poly_t* pa, const poly_t* pb);
poly_t* poly_add_frames_lazy(const poly_t* po, matrix_t* mat);
poly_t* poly_add_frame_lazy(const poly_t* po, const pkint_t* tab);

/* Linear Transformation (preserves minimality if inversible) */
poly_t* poly_assign_variable_old(const poly_t* po, int var, const pkint_t* tab);
poly_t* poly_substitute_variable_old(const poly_t* po, int var, const pkint_t* tab);
poly_t* poly_assign_variable(const poly_t* po, int var, const pkint_t* tab);
poly_t* poly_substitute_variable(const poly_t* po, int var, const pkint_t* tab);
poly_t* poly_assign_variables(const poly_t* po, const equation_t* eqn, int size);
poly_t* poly_substitute_variables(const poly_t* po, const equation_t* eqn, int size);

/* Widening (delivers non minimal polyhedra) */
poly_t* poly_widening(const poly_t* pa, const poly_t* pb);
poly_t* poly_limited_widening(const poly_t* pa, const poly_t* pb, matrix_t* mat);
/* Closure Operation */
poly_t* poly_closure(const poly_t* poly);
poly_t* poly_closure_lazy(const poly_t* poly);

#endif
