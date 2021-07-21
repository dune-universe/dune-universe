/*% $Id: internal.c,v 1.2 2003/12/12 14:03:49 bjeannet Exp $ */

/*% Global variables internal to the library. */

#include "config.h"
#include "internal.h"

/* Prefix pk\_xxx means that it used by the module xxx. */

pkint_t* pk_vector_pkintp = 0; /* Array of size maxcolumns */
pkint_t* pk_vector_tmp = 0;    /* Array of size 5 */

pkint_t** pk_matrix_pkintpp = 0; /* Array of size maxnbrows */
pkint_t pk_matrix_acc;
pkint_t pk_matrix_prod;

struct matrix_t* pk_cherni_bigray = 0;
struct satmat_t* pk_cherni_bigsatc = 0;
bitstring_t* pk_cherni_bitstringp = 0; /* Array of size bitindex_size(maxnbrows) */
int* pk_cherni_intp = 0; /* Array of size maxcolumns */
pkint_t pk_cherni_prod;

pkint_t* pk_poly_pkintp = 0; /* Array of size maxcolumns */
bitstring_t* pk_poly_bitstringp = 0; /* Array of size maxnbrows */
struct matrix_t* pk_poly_matspecial = 0;
pkint_t pk_poly_prod;
