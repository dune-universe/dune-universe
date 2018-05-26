/* File: lbfgs_stubs.c

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/signals.h>

#include "f2c.h"

#define FUN(name) ocaml_lbfgs_ ## name

/* Pointer to bigarray data.  BEWARE: C-style offsets. */
#define VEC_DATA_OFS(V) \
  ((double *) Caml_ba_array_val(v##V)->data) + Long_val(vOFS##V)

#define VEC_DATA(V) \
  ((double *) Caml_ba_array_val(v##V)->data)

#define INT_VEC_DATA(V) \
  ((int *) Caml_ba_array_val(v##V)->data)

#if defined(ARCH_INT64_TYPE) && defined(ARCH_BIG_ENDIAN)
/* If the integer are 64 bits, one must use to the higher bytes to get
   the least significant part of the number.  Use "int32" provided by
   OCaml "config.h". */
#define PTR_INT(x) (integer *) (((int32 *) &x) + 1)
#else
#define PTR_INT(x) &x /* low bits */
#endif


/*
 * Declaring Fortran functions
 **********************************************************************/

extern void setulb_(integer *n,        /* dimension of the problem */
                    integer *m,        /* metric corrections */
                    doublereal *x,     /* approximation to the solution */
                    doublereal *l,
                    doublereal *u,
                    integer *nbd,
                    doublereal *f,
                    doublereal *g,
                    doublereal *factr,
                    doublereal *pgtol,
                    doublereal *wa,
                    integer *iwa,
                    char *task,
                    integer *iprint,
                    char *csave,
                    logical *lsave,
                    integer *isave,
                    doublereal *dsave);

CAMLexport
value ocaml_lbfgs_setulb(value vn, value vm, value vOFSx, value vx,
                         value vOFSl, value vl, value vOFSu, value vu,
                         value vnbd,
                         value vf, value vg, value vfactr, value vpgtol,
                         value vwa, value viwa, value vtask, value viprint,
                         value vcsave, value vlsave, value visave,
                         value vdsave)
{
  integer n = Int_val(vn);
  integer m = Int_val(vm);
  doublereal f = Double_val(vf);
  doublereal factr = Double_val(vfactr);
  doublereal pgtol = Double_val(vpgtol);
  integer iprint = Int_val(viprint);

  setulb_(PTR_INT(n), PTR_INT(m),
          VEC_DATA_OFS(x), VEC_DATA_OFS(l), VEC_DATA_OFS(u), INT_VEC_DATA(nbd),
          &f, VEC_DATA(g), &factr, &pgtol, VEC_DATA(wa), INT_VEC_DATA(iwa),
          String_val(vtask), /* shared content with OCaml */
          PTR_INT(iprint), String_val(vcsave),
          INT_VEC_DATA(lsave), INT_VEC_DATA(isave), VEC_DATA(dsave));
  /* The following allocates but we do not need Caml arguments anymore: */
  return(copy_double(f));
}

CAMLexport
value ocaml_lbfgs_setulb_bc(value * argv, int argn)
{
  return ocaml_lbfgs_setulb(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
    argv[7], argv[8], argv[9], argv[10], argv[11], argv[12], argv[13],
    argv[14], argv[15], argv[16], argv[17], argv[18], argv[19], argv[20]);
}

