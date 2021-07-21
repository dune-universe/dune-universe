/*% $Id: vector.h,v 1.4 2004/02/27 10:30:43 bjeannet Exp $ */

/* This header file define operations on \emph{vectors}. A vector is just an
   array of elements of type \verb-pkint_t-, as a consequence functions need
   to be given also their size. */

#ifndef __POLKA_VECTOR_H__
#define __POLKA_VECTOR_H__

#include "config.h"
#include "polka.h"
#include "pkint.h"

/* Basic Operations */
pkint_t* vector_alloc(int size);
void     vector_free(pkint_t* q, int size);
void     vector_clear(pkint_t* const q, int size);
pkint_t* vector_copy(const pkint_t* q, int size);
void     vector_print(const pkint_t* q, int size);

/* Comparison \& Hashing */
int          vector_compare(const pkint_t* q1, const pkint_t* q2, int size);
int          vector_compare_expr(const pkint_t* q1, const pkint_t* q2, int size);
unsigned int vector_hash(const pkint_t* q, int size);

/* Normalization */
void vector_normalize(pkint_t* q, int size);
void vector_normalize_expr(pkint_t* q, int size);

/* Combination and Algebraic Operations */
void vector_combine(const pkint_t* q1, const pkint_t* q2, 
                    pkint_t* q3, int k, int size);
void vector_product(pkint_t* prod, 
                    const pkint_t* q1, const pkint_t* q2, int size);
void vector_product_strict(pkint_t* prod, 
			   const pkint_t* r1, const pkint_t* r2, int size);
void vector_add_expr(pkint_t* q3, 
		     const pkint_t* q1, const pkint_t* q2, int size);
void vector_sub_expr(pkint_t* q3, 
		     const pkint_t* q1, const pkint_t* q2, int size);
void vector_scale_expr(pkint_t* q2, 
		       pkint_t num, pkint_t den, const pkint_t* q1, int size);
void vector_add_dimensions(pkint_t* q2, const pkint_t* q1, int size, int dimsup);
void vector_add_dimensions_multi(pkint_t* q2, const pkint_t* q1, int size, 
				 const dimsup_t* tab, int multi);
void vector_remove_dimensions_multi(pkint_t* q2, const pkint_t* q1, int size, 
				    const dimsup_t* tab, int multi);

void vector_add_permute_dimensions (pkint_t* newq, 
				    const pkint_t* q, int size, 
				    int dimsup, const int * permut);
void vector_permute_remove_dimensions (pkint_t* newq, 
				    const pkint_t* q, int size, 
				    int dimsup, const int * permut);

/* Predicates that can be useful for users */
bool vector_is_null_strict(const pkint_t* q, int size);
bool vector_is_positivity_constraint(const pkint_t* q, int size);
bool vector_is_strictness_constraint(const pkint_t* q, int size);
bool vector_is_dummy_constraint(const pkint_t* q, int size);
/* Functions meant to be internal */
pkint_t* _vector_alloc_int(int size);

#endif
