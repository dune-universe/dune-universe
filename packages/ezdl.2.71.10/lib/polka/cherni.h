/*% $Id: cherni.h,v 1.3 2003/12/12 14:02:55 bjeannet Exp $ */

/* This header file define operations allowing to convert polyhedra 
from one representation to the dual one. */

#ifndef __POLKA_CHERNI_H__
#define __POLKA_CHERNI_H__

#include "config.h"
#include "polka.h"
#include "pkint.h"
#include "vector.h"
#include "satmat.h"
#include "matrix.h"

/* Functions meant to be internal */

void cherni_buildsatline(const matrix_t* con, const pkint_t* tab,
                         bitstring_t* satline);
int  cherni_conversion(matrix_t* con, const int start, 
                       matrix_t* ray, satmat_t* satc, int nbline);
int  cherni_simplify(matrix_t* con, matrix_t* ray, 
                     satmat_t* satf, const int nbline);

bool cherni_checksatmat(bool con_to_ray,
                        const matrix_t* C, const matrix_t* F, const satmat_t* satC);
bool cherni_checksat(bool con_to_ray,
                     const matrix_t* C, int nbequations,
                     const matrix_t* F, int nblines,
                     const satmat_t* satC);

void cherni_minimize(bool con_to_ray, matrix_t** p_C, 
                     matrix_t** p_F, satmat_t** p_satF,
                     int* p_nbeq, int* p_nbline);

void cherni_add_and_minimize(bool con_to_ray,
                             const matrix_t* Cdep, const matrix_t* Fdep,
                             const satmat_t* satCdep, const int nblines,
                             const matrix_t* Caut,
                             matrix_t** p_C, matrix_t** p_F, satmat_t** p_satF,
                             int* p_nbeq, int* p_nbline);

void cherni_add_dimensions(const matrix_t* C, const matrix_t* F, 
                           satmat_t** p_satC, const satmat_t* satF, 
			   int dimsup,
                           matrix_t** p_Cres, matrix_t** p_Fres, 
                           satmat_t** p_satCres);

void cherni_add_dimensions_multi(const matrix_t* C, const matrix_t* F, 
				 satmat_t** p_satC, const satmat_t* satF, 
				 const dimsup_t* tab, int size,
				 matrix_t** p_Cres, matrix_t** p_Fres, 
				 satmat_t** p_satCres,
				 int* p_dimsup);
void cherni_add_permute_dimensions(const matrix_t* C, const matrix_t* F, 
				   satmat_t** p_satC, const satmat_t* satF, 
				   int dimsup, const int* permutation,
				   matrix_t** p_Cres, matrix_t** p_Fres, 
				   satmat_t** p_satCres);

int gauss(matrix_t* con, int nbeq);
void backsubstitute(matrix_t* con, int rank);

#endif
