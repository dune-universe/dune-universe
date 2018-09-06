/* File: fftw3_stubs.c

   Objective Caml interface for FFTW.

   Copyright (C) 2005-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*/

#include <fftw3.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>

#define CAML_INTERNALS
#include <caml/bigarray.h>
#undef CAML_INTERNALS

#include <assert.h>
#define Assert(b) assert(b)
#include <string.h>

/* From recent config.h in OCaml sources. */
#ifndef HAS_STDINT_H
#ifndef ARCH_INT32_TYPE
#if SIZEOF_INT == 4
#define ARCH_UINT32_TYPE unsigned int
#elif SIZEOF_LONG == 4
#define ARCH_UINT32_TYPE unsigned long
#elif SIZEOF_SHORT == 4
#define ARCH_UINT32_TYPE unsigned short
#else
#error "No 32-bit integer type available"
#endif
#endif

typedef ARCH_UINT32_TYPE uint32_t;
#endif

#ifndef Caml_ba_kind_val
#define Caml_ba_kind_val(v) Int_val(v)
#endif
#ifndef Caml_ba_layout_val
/* No CAML_BA_LAYOUT_SHIFT in older OCaml versions. */
#define Caml_ba_layout_val(v) Int_val(v)
#endif


/*
 * Creating aligned Bigarray
 ***********************************************************************/
/* This section copies the bigarray definitions replacing malloc/free
   by the FFTW3 ones. */

#include "bigarray_stubs.c"

static void fftw3_caml_ba_finalize(value v)
{
  struct caml_bigarray * b = Bigarray_val(v);

  /* We do not have to distinguish according to BIGARRAY_MANAGED_MASK
     because it will always be BIGARRAY_MANAGED by creation. */
  if (b->proxy == NULL) {
    fftw_free(b->data);
  } else {
    if (-- b->proxy->refcount == 0) {
      fftw_free(b->proxy->data);
      caml_stat_free(b->proxy);
    }
  }
}

uintnat fftw3_caml_ba_deserialize(void * dst)
{
  struct caml_ba_array * b = dst;
  int i, elt_size;
  uintnat num_elts;

  /* Read back header information */
  b->num_dims = caml_deserialize_uint_4();
  b->flags = caml_deserialize_uint_4() | CAML_BA_MANAGED;
  b->proxy = NULL;
  for (i = 0; i < b->num_dims; i++) b->dim[i] = caml_deserialize_uint_4();
  /* Compute total number of elements */
  num_elts = caml_ba_num_elts(b);
  /* Determine element size in bytes */
  if ((b->flags & CAML_BA_KIND_MASK) >
#ifdef CAML_BA_CHAR
      CAML_BA_CHAR
#else
      CAML_BA_COMPLEX64
#endif
    )
    caml_deserialize_error("input_value: bad bigarray kind");
  elt_size = caml_ba_element_size[b->flags & CAML_BA_KIND_MASK];
  /* Allocate room for data */
  b->data = fftw_malloc(elt_size * num_elts);
  if (b->data == NULL)
    caml_deserialize_error("input_value: out of memory for bigarray");
  /* Read data */
  switch (b->flags & CAML_BA_KIND_MASK) {
#ifdef CAML_BA_CHAR
  case CAML_BA_CHAR:
#endif
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    caml_deserialize_block_1(b->data, num_elts); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    caml_deserialize_block_2(b->data, num_elts); break;
  case CAML_BA_FLOAT32:
  case CAML_BA_INT32:
    caml_deserialize_block_4(b->data, num_elts); break;
  case CAML_BA_COMPLEX32:
    caml_deserialize_block_4(b->data, num_elts * 2); break;
  case CAML_BA_FLOAT64:
  case CAML_BA_INT64:
    caml_deserialize_block_8(b->data, num_elts); break;
  case CAML_BA_COMPLEX64:
    caml_deserialize_block_8(b->data, num_elts * 2); break;
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
    caml_ba_deserialize_longarray(b->data, num_elts); break;
  }
  /* PR#5516: use C99's flexible array types if possible */
  return SIZEOF_BA_ARRAY + b->num_dims * sizeof(intnat);
}

static struct custom_operations fftw3_caml_ba_ops = {
  "fftw3_bigarray", /* identifier for serialization and deserialization */
  fftw3_caml_ba_finalize,
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  fftw3_caml_ba_deserialize
#ifdef custom_compare_ext_default
  , custom_compare_ext_default
#endif
};

/* TODO: register the struct custom_operations with the deserializer
 * using register_custom_operations  */

static value fftw3_caml_ba_alloc(int flags, int num_dims, intnat * dim)
{
  void * data = NULL;
  uintnat num_elts, asize, size;
  int overflow, i;
  value res;
  struct caml_ba_array * b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  Assert(num_dims >= 1 && num_dims <= CAML_BA_MAX_NUM_DIMS);
#ifdef CAML_BA_CHAR
  Assert((flags & CAML_BA_KIND_MASK) <= CAML_BA_CHAR);
#endif
  for (i = 0; i < num_dims; i++) dimcopy[i] = dim[i];
  size = 0;
  /* Data is allocated here (i.e. data == NULL in the original code). */
  overflow = 0;
  num_elts = 1;
  for (i = 0; i < num_dims; i++) {
    num_elts = caml_ba_multov(num_elts, dimcopy[i], &overflow);
  }
  size = caml_ba_multov(num_elts,
                        caml_ba_element_size[flags & CAML_BA_KIND_MASK],
                        &overflow);
  if (overflow) caml_raise_out_of_memory();
  data = fftw_malloc(size);
  if (data == NULL && size != 0) caml_raise_out_of_memory();
  flags |= CAML_BA_MANAGED;

  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  res = caml_alloc_custom(&fftw3_caml_ba_ops, asize, size, CAML_BA_MAX_MEMORY);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dimcopy[i];
  return res;
}

CAMLexport
value fftw3_ocaml_ba_create(value vkind, value vlayout, value vdim)
{
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  mlsize_t num_dims;
  int i, flags;

  num_dims = Wosize_val(vdim);
  if (num_dims < 1 || num_dims > CAML_BA_MAX_NUM_DIMS)
    caml_invalid_argument("Fftw3.Genarray.create: bad number of dimensions");
  for (i = 0; i < num_dims; i++) {
    dim[i] = Long_val(Field(vdim, i));
    if (dim[i] < 0)
      caml_invalid_argument("Fftw3.Genarray.create: negative dimension");
  }
  flags = Caml_ba_kind_val(vkind) | Caml_ba_layout_val(vlayout);
  return fftw3_caml_ba_alloc(flags, num_dims, dim);
}


/* FFTW3 stubs
 ***********************************************************************/

#define FFTW_RAISE_NO_FFTWF

#define FFTW(name) fftw_ ## name
#define FFTW_OCAML(name) fftw_ocaml_ ## name
#define FLOAT double
#define PREC "D"
#include "fftw3SD_stubs.c"
#undef PREC
#undef FLOAT
#undef FFTW_OCAML
#undef FFTW

#ifdef FFTW3F_EXISTS
#define FFTW(name) fftwf_ ## name
#define FLOAT float
#else
/* Single precision version not present.  Use the double precision (to
   type check) but raise an exception when calling any FFTW function */
#define FFTW(name) fftw_ ## name
#define FLOAT double /* only to typecheck */
#undef FFTW_RAISE_NO_FFTWF
#define FFTW_RAISE_NO_FFTWF \
  caml_failwith("Fftw3.S: single precision C fftwf library not present")
#endif /* FFTW3F_EXISTS */

#define FFTW_OCAML(name) fftwf_ocaml_ ## name /* single precision stubs */
#define PREC "S"
#include "fftw3SD_stubs.c"
#undef PREC
#undef FLOAT
#undef FFTW_OCAML
#undef FFTW

#undef FFTW_RAISE_NO_FFTWF

/* Wisdom
 ***********************************************************************/

CAMLexport
value fftw3_ocaml_export_wisdom_to_file(value fname)
{
  CAMLparam1(fname);
  FILE * fh;

  fh = fopen(String_val(fname), "w");
  fftw_export_wisdom_to_file(fh);
  fclose(fh);

  CAMLreturn(Val_unit);
}

CAMLexport
value fftw3_ocaml_export_wisdom_to_string(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(wisdom);
  char *w;

  w = fftw_export_wisdom_to_string();
  wisdom = caml_copy_string(w);
  fftw_free(w);
  CAMLreturn(wisdom);
}

static void fftw3_ocaml_export_write(char c, void* data)
{
  static value* write = NULL;
  if (write == NULL)
    /* First time around, look up by name */
    write = caml_named_value("fftw3_wisdom_export");
  caml_callback(*write, Val_int(c));
}


CAMLexport
value fftw3_ocaml_export_wisdom(value unit)
{
  CAMLparam1(unit);
  fftw_export_wisdom(&fftw3_ocaml_export_write, NULL);
  CAMLreturn(Val_unit);
}


CAMLexport
value fftw3_ocaml_import_system_wisdom(value unit)
{
  CAMLparam1(unit);
  int r;

  r = fftw_import_system_wisdom();
  if(r == 0) caml_failwith("Fftw3.Wisdom.from_system");
  CAMLreturn(Val_unit);
}

CAMLexport
value fftw3_ocaml_import_wisdom_from_file(value fname)
{
  CAMLparam1(fname);
  FILE* fh;
  int r;

  fh = fopen(String_val(fname), "r");
  r = fftw_import_wisdom_from_file(fh);
  fclose(fh);
  if(r == 0) caml_failwith("Fftw3.Wisdom.from_file");
  CAMLreturn(Val_unit);
}

CAMLexport
value fftw3_ocaml_import_wisdom_from_string(value s)
{
  CAMLparam1(s);
  int r;

  r = fftw_import_wisdom_from_string(String_val(s));
  if(r == 0) caml_failwith("Fftw3.Wisdom.from_string");
  CAMLreturn(Val_unit);
}

static int fftw3_ocaml_import_wisdom_read(void* data)
{
  static value* read = NULL;
  if (read == NULL)
    /* First time around, look up by name */
    read = caml_named_value("fftw3_wisdom_import");
  return(Int_val(caml_callback(*read, Val_unit)));
}


CAMLexport
value fftw3_ocaml_import_wisdom(value unit)
{
  CAMLparam1(unit);
  int r;
  r = fftw_import_wisdom(fftw3_ocaml_import_wisdom_read, NULL);
  if(r == 0) caml_failwith("Fftw3.Wisdom.import");
  CAMLreturn(Val_unit);
}

CAMLexport
value fftw3_ocaml_forget_wisdom(value unit)
{
  CAMLparam1(unit);
  fftw_forget_wisdom();
  CAMLreturn(Val_unit);
}
