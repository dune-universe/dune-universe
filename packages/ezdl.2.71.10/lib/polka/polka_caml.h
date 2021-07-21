/* $Id: polka_caml.h,v 1.5 2004/02/27 10:34:11 bjeannet Exp $ */

#ifndef __POLKA_CAML_H__
#define __POLKA_CAML_H__

#include "polka.h"
#include "vector.h"
#include "matrix.h"
#include "poly.h"

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/fail.h"

// extern IID IID_IUnknown;
//extern IID IID_IUnknown, IID_IX, IID_IY;
extern const int camlidl_polka_integer;

static inline void int_set_pkint(long* pres, pkint_t x, bool clear)
{
  if (pkint_cmp_si(x,Max_long)>0 || pkint_cmp_si(x,Min_long)<0){
    size_t size = pkint_sizeinbase10(x);
    char* str = malloc(size+2);
    (void)pkint_get_str10(str, x);
    value v = caml_copy_string(str);
    free(str);
    if (clear){ pkint_clear(x); }
    caml_raise_with_arg(*caml_named_value("camlidl_polka_overflow"),v);
  }
  else {
    *pres = pkint_get_si(x);
    if (clear){ pkint_clear(x); }
  }
}

typedef struct vector__t { pkint_t * q; int size; } vector__t;
typedef matrix_t* matrix__t;
typedef poly_t* poly__t;

extern struct custom_operations camlidl_polka_vector_custom;
extern struct custom_operations camlidl_polka_matrix_custom;
extern struct custom_operations camlidl_polka_poly_custom;

value camlidl_polka_dimsup_c2ml(dimsup_t*);
value camlidl_polka_vector_c2ml(struct vector__t* vs);
value camlidl_polka_matrix_c2ml(matrix_t** pmat);
value camlidl_polka_poly_c2ml(struct poly_t** ppoly);
value camlidl_polka_equation_c2ml(equation_t*);

void  camlidl_polka_dimsup_ml2c(value vdimsup, dimsup_t*);
void  camlidl_polka_vector_ml2c(value vvec, struct vector__t *vs);
void  camlidl_polka_matrix_ml2c(value vmat, matrix_t** pmat);
void  camlidl_polka_poly_ml2c(value vpoly, struct poly_t** ppoly);
void  camlidl_polka_equation_ml2c(value vequation, equation_t*);

extern mlsize_t camlidl_polka_heap;
extern pkint_t* camlidl_polka_tmp;
value camlidl_polka_initialize(value vstrict, value vmaxdims, value vmaxrows);
value camlidl_polka_finalize(value a);
value camlidl_polka_set_widening_affine(value dummy);
value camlidl_polka_set_widening_linear(value dummy);
value camlidl_polka_version(value a);

value camlidl_polka_poly_constraints(value _v_po);
value camlidl_polka_poly_frames(value _v_po);

#endif
