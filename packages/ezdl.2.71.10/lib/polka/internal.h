/*% $Id: internal.h,v 1.2 2003/12/12 14:03:49 bjeannet Exp $ */

/* Header file for access to global variables internal to the library. */


#ifndef __POLKA_INTERNAL_H__
#define __POLKA_INTERNAL_H__

#include "config.h"
#include "polka.h"
#include "bit.h"
#include "pkint.h"

/* These variables are used by various functions.  The prefix pk\_XXX
   indicates that the variable is used by the module XXX. */

extern pkint_t* pk_vector_pkintp;
extern pkint_t* pk_vector_tmp;

extern pkint_t** pk_matrix_pkintpp;
extern pkint_t pk_matrix_acc;
extern pkint_t pk_matrix_prod;

extern struct matrix_t* pk_cherni_bigray;
extern struct satmat_t* pk_cherni_bigsatc;
extern bitstring_t* pk_cherni_bitstringp;
extern int* pk_cherni_intp;
extern pkint_t pk_cherni_prod;

extern pkint_t* pk_poly_pkintp;
extern bitstring_t* pk_poly_bitstringp;
extern struct matrix_t* pk_poly_matspecial;
extern pkint_t pk_poly_prod;

#endif
