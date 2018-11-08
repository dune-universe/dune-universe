/* File: odepack_stubs.c

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
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

#define CALL(name) d ## name ## _
#define SUBROUTINE(name) extern void CALL(name)
#define FUN(name) ocaml_odepack_d ## name

typedef doublereal* vec;
typedef doublereal* mat; /* fortran (columnwise) layout */
typedef integer* int_vec;
typedef void (*VEC_FIELD)(integer*, doublereal*, vec, vec);
typedef void (*JACOBIAN)(integer*, doublereal*, vec,
                         integer*, integer*, doublereal*, integer*);

/* Fetch vector parameters from bigarray */
#define VEC_PARAMS(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  double *V##_data = ((double *) big_##V->data) /*+ (Long_val(vOFS##V) - 1)*/

#define VEC_DATA(V) \
  ((double *) Caml_ba_array_val(v##V)->data)

/* FORTRAN INTEGER data declaration without a kind parameter = 4 bytes */
#define INT_VEC_PARAMS(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  integer *V##_data = ((integer *) big_##V->data) /*+ (Long_val(vOFS##V) - 1)*/

#define INT_VEC_DATA(V) \
  ((int *) Caml_ba_array_val(v##V)->data)


/*
 * Declaring Fortran functions
 **********************************************************************/

/* The original FORTRAN name was XSETF (changed by script). */
extern void odepack_xsetf_(integer* MFLAG);

SUBROUTINE(lsode)(VEC_FIELD F,
                  integer *NEQ, /*  Number of first-order ODE's. */
                  vec Y,
                  doublereal *T,
                  doublereal *TOUT,
                  integer *ITOL,
                  doublereal *RTOL, /* Relative tolerance */
                  doublereal *ATOL, /* absolute tolerance, scalar or array */
                  integer *ITASK,   /* task to perform */
                  integer *ISTATE,  /* specify the state of the calculation */
                  integer *IOPT,    /* whether optional inputs are used */
                  vec RWORK, /* of size */ integer *LRW,
                  int_vec IWORK, /* of size */ integer *LIW,
                  JACOBIAN JAC, /* optional subroutine for Jacobian matrix */
                  integer *MF);

SUBROUTINE(lsodes)(VEC_FIELD F,
                  integer *NEQ, /*  Number of first-order ODE's. */
                  vec Y,
                  doublereal *T,
                  doublereal *TOUT,
                  integer *ITOL,
                  doublereal *RTOL, /* Relative tolerance */
                  doublereal *ATOL, /* absolute tolerance, scalar or array */
                  integer *ITASK,   /* task to perform */
                  integer *ISTATE,  /* specify the state of the calculation */
                  integer *IOPT,    /* whether optional inputs are used */
                  vec RWORK, /* of size */ integer *LRW,
                  int_vec IWORK, /* of size */ integer *LIW,
                  JACOBIAN JAC, /* optional subroutine for Jacobian matrix */
                  integer *MF);

SUBROUTINE(lsoda)(VEC_FIELD F,
                  integer *NEQ, /* size of the ODE system */
                  vec Y,
                  doublereal *T,
                  doublereal *TOUT,
                  integer *ITOL,
                  doublereal *RTOL,
                  doublereal *ATOL,
                  integer *ITASK,
                  integer *ISTATE,
                  integer *IOPT,
                  vec RWORK, /* of size */ integer *LRW,
                  int_vec IWORK, /* of size */ integer *LIW,
                  JACOBIAN JAC,
                  integer *JT);

SUBROUTINE(lsodar)(VEC_FIELD F,
                   integer *NEQ, /* size of the ODE system */
                   vec Y,
                   doublereal *T,
                   doublereal *TOUT,
                   integer *ITOL,
                   doublereal *RTOL,
                   doublereal *ATOL,
                   integer *ITASK,
                   integer *ISTATE,
                   integer *IOPT,
                   vec RWORK, /* of size */ integer *LRW,
                   int_vec IWORK, /* of size */ integer *LIW,
                   JACOBIAN JAC,
                   integer *JT,
                   void (*G)(integer*, doublereal*, vec, integer*, vec),
                   integer *NG,  /* number of functions */
                   integer *JROOT);

SUBROUTINE(lsodpk)(VEC_FIELD F,
                   integer *NEQ, /*  Number of first-order ODE's. */
                   vec Y,
                   doublereal *T,
                   doublereal *TOUT,
                   integer *ITOL,
                   doublereal *RTOL, /* Relative tolerance */
                   doublereal *ATOL, /* absolute tolerance, scalar or array */
                   integer *ITASK,   /* task to perform */
                   integer *ISTATE,  /* specify the state of the calculation */
                   integer *IOPT,    /* whether optional inputs are used */
                   vec RWORK, /* of size */ integer *LRW,
                   int_vec IWORK, /* of size */ integer *LIW,
                   JACOBIAN JAC, /* optional subroutine for Jacobian matrix */
                   void (*PSOL)(integer*, doublereal*, vec, vec),
                   integer *MF);

// DLSODKR, DLSODI, DLSOIBT, DLSODIS

/*
 * Bindings
 **********************************************************************/

CAMLexport
value ocaml_odepack_xsetf(value vflag)
{
  /* noalloc */
  integer mflag = Int_val(vflag);
  odepack_xsetf_(&mflag);
  return Val_unit;
}

/* Since NEQ may be an array (with only NEQ(1) used by LSODA), one
 * will use it to: pass to the function evaluating the Caml closure,
 * pass the bigarray Y (to avoid recreating it) and pass a bigarray
 * structure to YDOT (created on the first call),...  */
static void eval_vec_field(integer* NEQ, doublereal* T, vec Y, vec YDOT)
{
  CAMLparam0();
  CAMLlocal1(vT);
  value **pvNEQ = (value **) NEQ;
  value *closure_f = pvNEQ[1];
  value *vY = pvNEQ[2]; /* data location is always the same */
  value *vYDOT = pvNEQ[3];

  /* FIXME: Use PPX [@unboxed] to avoid this allocation. */
  vT = caml_copy_double(*T); /* allocates! */
  Caml_ba_array_val(*vYDOT)->data = YDOT; /* update RWORK location */
  caml_callback3(*closure_f, vT, *vY, *vYDOT);
  CAMLreturn0;
}

static void eval_jac(integer* NEQ, doublereal* T, vec Y,
                     integer* ML, integer* MU, mat PD, integer* NROWPD)
{
  CAMLparam0();
  CAMLlocal1(vT);
  value **pvNEQ = (value **) NEQ;
  value *closure_jac = pvNEQ[4];
  value *vPD = pvNEQ[5];
  value args[4];

  /* FIXME: Use PPX [@unboxed] to avoid this allocation. */
  vT = caml_copy_double(*T);
  /* No alloc anymore: can store the dereferenced pointers. */
  args[0] = vT;
  args[1] = *(pvNEQ[2]); /* vY */
  args[2] = Val_int(*MU + 1); /* vd = MU+1, row corresponding to diagonal */
  args[3] = *vPD; /* vPD */
  Caml_ba_array_val(*vPD)->data = PD; /* update location */
  caml_callbackN(*closure_jac, 4, args);
  CAMLreturn0;
}


CAMLexport
value ocaml_odepack_set_iwork(value vIWORK, value vML, value vMU,
                              value vIXPR, value vMXSTEP)
{
  /* noalloc */
  int *IWORK_data = INT_VEC_DATA(IWORK);
  IWORK_data[0] = Int_val(vML);
  IWORK_data[1] = Int_val(vMU);
  IWORK_data[4] = (Bool_val(vIXPR))? 1 : 0;
  IWORK_data[5] = Int_val(vMXSTEP);
  IWORK_data[6] = 0; /* MXHNIL */
  IWORK_data[7] = 0; /* MXORDN */
  IWORK_data[8] = 0; /* MXORDS */
  return Val_unit;
}

CAMLexport
value FUN(lsoda)(value vf, value vY, value vT, value vTOUT,
                 value vITOL, value vRTOL, value vATOL, value vITASK,
                 value vISTATE, value vRWORK, value vIWORK,
                 value vJAC, value vJT,  value vYDOT, value vPD)
{
  CAMLparam5(vf, vY, vT, vTOUT, vITOL);
  CAMLxparam5(vRTOL, vATOL, vITASK, vISTATE, vRWORK);
  CAMLxparam5(vIWORK, vJAC, vJT, vYDOT, vPD);
  VEC_PARAMS(Y);
  value *NEQ[6];
  doublereal T = Double_val(vT), TOUT = Double_val(vTOUT);
  integer ITOL = Int_val(vITOL);
  integer ITASK = Int_val(vITASK) + 1;
  integer ISTATE = Int_val(vISTATE);
  integer IOPT = 1;
  VEC_PARAMS(RWORK);
  INT_VEC_PARAMS(IWORK);
  integer JT = Int_val(vJT);

  /* One needs to pass the protected values by reference because the
     GC can change their value (= the location of the data pointed to). */
  ((integer *) NEQ)[0] = dim_Y;
  NEQ[1] = &vf;
  NEQ[2] = &vY;
  NEQ[3] = &vYDOT;
  NEQ[4] = &vJAC;
  NEQ[5] = &vPD;

  CALL(lsoda)(&eval_vec_field, (integer *) NEQ, Y_data, &T, &TOUT,
              &ITOL, VEC_DATA(RTOL), VEC_DATA(ATOL), &ITASK, &ISTATE, &IOPT,
              RWORK_data, &dim_RWORK,  IWORK_data, &dim_IWORK,
              &eval_jac, &JT);

  CAMLreturn(Val_int(ISTATE));
}

CAMLexport
value FUN(lsoda_bc)(value * argv, int argn)
{
  return FUN(lsoda)(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                    argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
                    argv[12], argv[13], argv[14]);
}


static void eval_constraint(integer* NEQ, doublereal* T, vec Y,
                            integer* NG_, vec GOUT)
{
  CAMLparam0();
  CAMLlocal1(vT);
  value **pvNEQ = (value **) NEQ;
  value *closure_g = pvNEQ[6];
  value *vY = pvNEQ[2]; /* data location is always the same */
  value *vGOUT = pvNEQ[7];

  vT = caml_copy_double(*T); /* allocates! */
  Caml_ba_array_val(*vGOUT)->data = GOUT; /* update location */
  caml_callback3(*closure_g, vT, *vY, *vGOUT);
  CAMLreturn0;
}

CAMLexport
value FUN(lsodar)(value vf, value vY, value vT, value vTOUT,
                  value vITOL, value vRTOL, value vATOL, value vITASK,
                  value vISTATE, value vRWORK, value vIWORK,
                  value vJAC, value vJT,  value vYDOT, value vPD,
                  value vg, value vGOUT, value vJROOT)
{
  CAMLparam5(vf, vY, vT, vTOUT, vITOL);
  CAMLxparam5(vRTOL, vATOL, vITASK, vISTATE, vRWORK);
  CAMLxparam5(vIWORK, vJAC, vJT, vYDOT, vPD);
  CAMLxparam3(vg, vGOUT, vJROOT);
  CAMLlocal1(vcouple);
  VEC_PARAMS(Y);
  value *NEQ[8];
  doublereal T = Double_val(vT), TOUT = Double_val(vTOUT);
  integer ITOL = Int_val(vITOL);
  integer ITASK = Int_val(vITASK) + 1;
  integer ISTATE = Int_val(vISTATE);
  integer IOPT = 1;
  VEC_PARAMS(RWORK);
  INT_VEC_PARAMS(IWORK);
  integer JT = Int_val(vJT);
  INT_VEC_PARAMS(JROOT);

  /* One needs to pass the protected values by reference because the
     GC can change their value (= the location of the data pointed to). */
  ((integer *) NEQ)[0] = dim_Y;
  NEQ[1] = &vf;
  NEQ[2] = &vY;
  NEQ[3] = &vYDOT;
  NEQ[4] = &vJAC;
  NEQ[5] = &vPD;
  NEQ[6] = &vg;
  NEQ[7] = &vGOUT; /* used by eval_constraint */

  CALL(lsodar)(&eval_vec_field, (integer *) NEQ, Y_data, &T, &TOUT,
               &ITOL, VEC_DATA(RTOL), VEC_DATA(ATOL), &ITASK, &ISTATE, &IOPT,
               RWORK_data, &dim_RWORK,  IWORK_data, &dim_IWORK,
               &eval_jac, &JT,
               &eval_constraint, &dim_JROOT, JROOT_data);

  /* T may be modified on exit -- useful if stopped at a root. */
  vT = caml_copy_double(T); /* reuse vT */
  vcouple = caml_alloc_small(2, 0);
  Field(vcouple, 0) = Val_int(ISTATE);
  Field(vcouple, 1) = vT;
  CAMLreturn(vcouple);
}

CAMLexport
value FUN(lsodar_bc)(value * argv, int argn)
{
  return FUN(lsodar)(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                     argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
                     argv[12], argv[13], argv[14], argv[15], argv[16],
                     argv[17]);
}
