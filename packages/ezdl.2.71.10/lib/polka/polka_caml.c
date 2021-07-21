/* $Id: polka_caml.c,v 1.5 2004/02/27 10:34:11 bjeannet Exp $ */

#include <stddef.h>
#include <string.h>
#include <assert.h>
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/camlidlruntime.h"

#include "polka_caml.h"

const int camlidl_polka_integer = POLKA_NUM;

mlsize_t camlidl_polka_heap = 64<<20;; /* 64MegaCoeff by default */
pkint_t* camlidl_polka_tmp = 0;

/* ========================================================================== */
/* \section{dimsup} */
/* ========================================================================== */

/* Should never be used, as no C function generates an object of type dimsup_t. */
value camlidl_polka_dimsup_c2ml(dimsup_t * dimsup)
{
  fprintf(stderr,"Internal error. polka_caml.c: camlidl_polka_dimsup_c2ml\n");
  abort();
}

void camlidl_polka_dimsup_ml2c(value vdimsup, dimsup_t * dimsup)
{
  value vfield;
  vfield = Field(vdimsup, 0);
  dimsup->pos = Int_val(vfield);
  vfield = Field(vdimsup, 1);
  dimsup->nbdims = Int_val(vfield);
}

/* ========================================================================== */
/* \section{equation} */
/* ========================================================================== */

/* Should never be used, as no C function generates an object of type equation_t. */

value camlidl_polka_equation_c2ml(equation_t * dimsup)
{
  fprintf(stderr,"Internal error. polka_caml.c: camlidl_polka_equation_c2ml\n");
  abort();
}

void camlidl_polka_equation_ml2c(value vequation, struct equation_t* equation) \
{
  value vfield;
  struct vector__t vs;
  vfield = Field(vequation, 0);
  equation->var = Int_val(vfield);
  vfield = Field(vequation, 1);
  camlidl_polka_vector_ml2c(vfield, &vs);
  equation->expr = vs.q;
}

/* ========================================================================== */
/* \section{Vector} */
/* ========================================================================== */

static void camlidl_polka_vector_finalize(value val)
{
  size_t size = ((struct vector__t *)Data_custom_val(val))->size;
  pkint_t* q = ((struct vector__t *)Data_custom_val(val))->q;
  if (size > 0) vector_free(q, size);
}
static int camlidl_polka_vector_compare(value vala, value valb)
{
  CAMLparam2(vala,valb);
  size_t sizea = ((struct vector__t *)Data_custom_val(vala))->size;
  size_t sizeb = ((struct vector__t *)Data_custom_val(valb))->size;
  pkint_t* qa = ((struct vector__t *)Data_custom_val(vala))->q;
  pkint_t* qb = ((struct vector__t *)Data_custom_val(valb))->q;
  int res;
  if (sizea != sizeb)
    res = (int)sizea-(int)sizeb;
  else {
    size_t i;
    res = 0;
    for (i=0; i<sizea; i++){
      res = pkint_cmp(qa[i],qb[i]);
      if (res!=0) break;
    }
  }
  CAMLreturn(res);
}
static long camlidl_polka_vector_hash(value val)
{
  CAMLparam1(val);
  long res =  (long)vector_hash(((struct vector__t *)Data_custom_val(val))->q,
				((struct vector__t *)Data_custom_val(val))->size);
  CAMLreturn(res);
}

struct custom_operations camlidl_polka_vector_custom = {
  "vector_caml_custom_operations",
  &camlidl_polka_vector_finalize,
  &camlidl_polka_vector_compare,
  &camlidl_polka_vector_hash,
  custom_serialize_default,
  custom_deserialize_default
};

value camlidl_polka_vector_c2ml(struct vector__t* vs)
{
  value vvec;
  vvec = alloc_custom(&camlidl_polka_vector_custom, sizeof(struct vector__t),
		      (unsigned int)(vs->size),camlidl_polka_heap);
  ((struct vector__t *)Data_custom_val(vvec))->q = vs->q;
  ((struct vector__t *)Data_custom_val(vvec))->size = vs->size;
  return vvec;
}

void camlidl_polka_vector_ml2c(value vvec, struct vector__t *vs)
{
  vs->q = ((struct vector__t *)Data_custom_val(vvec))->q;
  vs->size = ((struct vector__t *)Data_custom_val(vvec))->size;
}

/* ========================================================================== */
/* \section{Matrix} */
/* ========================================================================== */

static void camlidl_polka_matrix_finalize(value val)
{
  matrix_free(*((matrix_t **)Data_custom_val(val)));
}
static int camlidl_polka_matrix_compare(value vala, value valb)
{
  CAMLparam2(vala,valb);
  matrix_t* ma = *(matrix_t **)Data_custom_val(vala);
  matrix_t* mb = *(matrix_t **)Data_custom_val(valb);
  int i,j,res;
  res = ma->nbrows - mb->nbrows;
  if (res==0){
    res = ma->nbcolumns - mb->nbcolumns;
    if (res==0){
      if (ma->_pinit && mb->_pinit){
	for (i=0; i<ma->nbrows; i++){
	  for (j=0; j<ma->nbcolumns; j++){
	    res = pkint_cmp(ma->p[i][j],mb->p[i][j]);
	    if (res) goto compare_exit;
	  }
	}
      }
    }
  }
 compare_exit:
  CAMLreturn(res);
}
static long camlidl_polka_matrix_hash(value val)
{
  CAMLparam1(val);
  long res = (long)matrix_hash(*(matrix_t **)Data_custom_val(val));
  CAMLreturn(res);
}
struct custom_operations camlidl_polka_matrix_custom = {
  "matrix_caml_custom_operations",
  &camlidl_polka_matrix_finalize,
  &camlidl_polka_matrix_compare,
  &camlidl_polka_matrix_hash,
  custom_serialize_default,
  custom_deserialize_default
};

value camlidl_polka_matrix_c2ml(matrix_t** pmat)
{
  matrix_t* mat = *pmat;
  value vmat = alloc_custom(&camlidl_polka_matrix_custom, sizeof(struct matrix_t *),
			    (unsigned int)(mat->_maxrows*mat->nbcolumns),
			    camlidl_polka_heap);
  *(matrix_t**)Data_custom_val(vmat) = mat;
  return vmat;
}

void camlidl_polka_matrix_ml2c(value vmat, matrix_t** pmat){
  *pmat = *(matrix_t**)Data_custom_val(vmat);
}

/* ========================================================================== */
/* \section{Poly} */
/* ========================================================================== */

static void camlidl_polka_poly_finalize(value val)
{
  poly_free(*((poly_t **)Data_custom_val(val)));
}
struct custom_operations camlidl_polka_poly_custom = {
  "poly_caml_custom_operations",
  &camlidl_polka_poly_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value camlidl_polka_poly_c2ml(struct poly_t** ppoly)
{
  poly_t* poly = *ppoly;
  size_t size =
    ((poly->C ? poly->C->_maxrows : 0) + (poly->F ? poly->F->_maxrows : 0)) *
    (polka_dec+poly->dim);

  value vpoly = alloc_custom(&camlidl_polka_poly_custom, sizeof(struct poly_t *),
			     (unsigned int)size,
			     camlidl_polka_heap);
  *(poly_t**)Data_custom_val(vpoly) = poly;
  return vpoly;
}
void camlidl_polka_poly_ml2c(value vpoly, struct poly_t** ppoly)
{
  *ppoly = *((poly_t**)Data_custom_val(vpoly));
}

value camlidl_polka_poly_constraints(value _v_po)
{
  CAMLparam1(_v_po); CAMLlocal2(_v_res,_v_mat);
  struct poly_t *po;
  struct matrix_t* mat;

  camlidl_polka_poly_ml2c(_v_po,&po);
  mat = po->C;
  if (mat==0){
    _v_res = Val_int(0);
  }
  else {
    matrix_t* nmat = matrix_copy(mat);
    _v_mat = camlidl_polka_matrix_c2ml(&nmat);
    _v_res = alloc_small(1,0);
    Field(_v_res,0) = _v_mat;
  }
  CAMLreturn(_v_res);
}

value camlidl_polka_poly_frames(value _v_po)
{
  CAMLparam1(_v_po); CAMLlocal2(_v_res,_v_mat);
  poly_t* po;
  matrix_t* mat;
  camlidl_polka_poly_ml2c(_v_po,&po);
  mat = po->F;
  if (mat==0){
    _v_res = Val_int(0);
  }
  else {
    matrix_t* nmat = matrix_copy(mat);
    _v_mat = camlidl_polka_matrix_c2ml(&nmat);
    _v_res = alloc_small(1,0);
    Field(_v_res,0) = _v_mat;
  }
  CAMLreturn(_v_res);
}


/* ========================================================================== */
/* \section{Polka} */
/* ========================================================================== */

value
camlidl_polka_initialize(value vstrict,
			 value vmaxdims, value vmaxrows)
{
  CAMLparam3(vstrict,vmaxdims,vmaxrows);
  int strict;
  size_t maxdims, maxrows;

  strict = Bool_val(vstrict);
  maxdims = Int_val(vmaxdims);
  maxrows = Int_val(vmaxrows);
  if (polka_integer!=camlidl_polka_integer){
    fprintf(stderr,"Polka.initialize: you didn't compile the polka OCaml interface and the polka C library with the same integer type, or you linked incompatible versions !\n");
    exit(-1);
  }
  polka_initialize(strict,maxdims,maxrows);
  /*  camlidl_polka_tmp */
  camlidl_polka_tmp = vector_alloc(2);
  CAMLreturn(Val_unit);
}

value camlidl_polka_set_gc(value vmax)
{
  CAMLparam1(vmax);
  camlidl_polka_heap = Int_val(vmax);
  CAMLreturn(Val_unit);
}

value camlidl_polka_finalize(value a)
{
  CAMLparam1(a);
  if (!camlidl_polka_tmp){
    vector_free(camlidl_polka_tmp,2);
    camlidl_polka_tmp = 0;
  }
  polka_finalize();
  CAMLreturn(Val_unit);
}

value camlidl_polka_set_widening_affine(value dummy)
{
  CAMLparam1(dummy);
  polka_set_widening_affine();
  CAMLreturn(Val_unit);
}
value camlidl_polka_set_widening_linear(value dummy)
{
  CAMLparam1(dummy);
  polka_set_widening_linear();
  CAMLreturn(Val_unit);
}
