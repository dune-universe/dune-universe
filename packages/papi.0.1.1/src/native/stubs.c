/* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md */

#include <papi.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define __unit value unit __attribute__((unused))
#define __int_option(x, def) (Is_block (x) ? Long_val (Field (x, 0)) : def)

/* Ordering must match the ordering of `event' constructors! */
static int event_table [] = {
  PAPI_L1_DCM, PAPI_L1_ICM, PAPI_L2_DCM, PAPI_L2_ICM, PAPI_L3_DCM, PAPI_L3_ICM,
  PAPI_L1_TCM, PAPI_L2_TCM, PAPI_L3_TCM, PAPI_CA_SNP, PAPI_CA_SHR, PAPI_CA_CLN,
  PAPI_CA_INV, PAPI_CA_ITV, PAPI_L3_LDM, PAPI_L3_STM, PAPI_BRU_IDL,
  PAPI_FXU_IDL, PAPI_FPU_IDL, PAPI_LSU_IDL, PAPI_TLB_DM, PAPI_TLB_IM,
  PAPI_TLB_TL, PAPI_L1_LDM, PAPI_L1_STM, PAPI_L2_LDM, PAPI_L2_STM, PAPI_BTAC_M,
  PAPI_PRF_DM, PAPI_L3_DCH, PAPI_TLB_SD, PAPI_CSR_FAL, PAPI_CSR_SUC,
  PAPI_CSR_TOT, PAPI_MEM_SCY, PAPI_MEM_RCY, PAPI_MEM_WCY, PAPI_STL_ICY,
  PAPI_FUL_ICY, PAPI_STL_CCY, PAPI_FUL_CCY, PAPI_HW_INT, PAPI_BR_UCN,
  PAPI_BR_CN, PAPI_BR_TKN, PAPI_BR_NTK, PAPI_BR_MSP, PAPI_BR_PRC, PAPI_FMA_INS,
  PAPI_TOT_IIS, PAPI_TOT_INS, PAPI_INT_INS, PAPI_FP_INS, PAPI_LD_INS,
  PAPI_SR_INS, PAPI_BR_INS, PAPI_VEC_INS, PAPI_RES_STL, PAPI_FP_STAL,
  PAPI_TOT_CYC, PAPI_LST_INS, PAPI_SYC_INS, PAPI_L1_DCH, PAPI_L2_DCH,
  PAPI_L1_DCA, PAPI_L2_DCA, PAPI_L3_DCA, PAPI_L1_DCR, PAPI_L2_DCR, PAPI_L3_DCR,
  PAPI_L1_DCW, PAPI_L2_DCW, PAPI_L3_DCW, PAPI_L1_ICH, PAPI_L2_ICH, PAPI_L3_ICH,
  PAPI_L1_ICA, PAPI_L2_ICA, PAPI_L3_ICA, PAPI_L1_ICR, PAPI_L2_ICR, PAPI_L3_ICR,
  PAPI_L1_ICW, PAPI_L2_ICW, PAPI_L3_ICW, PAPI_L1_TCH, PAPI_L2_TCH, PAPI_L3_TCH,
  PAPI_L1_TCA, PAPI_L2_TCA, PAPI_L3_TCA, PAPI_L1_TCR, PAPI_L2_TCR, PAPI_L3_TCR,
  PAPI_L1_TCW, PAPI_L2_TCW, PAPI_L3_TCW, PAPI_FML_INS, PAPI_FAD_INS,
  PAPI_FDV_INS, PAPI_FSQ_INS, PAPI_FNV_INS, PAPI_FP_OPS, PAPI_SP_OPS,
  PAPI_DP_OPS, PAPI_VEC_SP, PAPI_VEC_DP, PAPI_REF_CYC,
  /* PAPI_END */
};

/* Ordering must match the ordering of `error' constructors! */
static int error_table [] = {
  PAPI_EINVAL, PAPI_ENOMEM, PAPI_ESYS, PAPI_ECMP,
  PAPI_ECLOST, PAPI_EBUG, PAPI_ENOEVNT, PAPI_ECNFLCT, PAPI_ENOTRUN, PAPI_EISRUN,
  PAPI_ENOEVST, PAPI_ENOTPRESET, PAPI_ENOCNTR, PAPI_EMISC, PAPI_EPERM, PAPI_ENOINIT,
  PAPI_ENOCMP, PAPI_ENOSUPP, PAPI_ENOIMPL, PAPI_EBUF, PAPI_EINVAL_DOM, PAPI_EATTR,
  PAPI_ECOUNT, PAPI_ECOMBO,
};

__attribute__ ((__noreturn__))
static void caml_raise_sys_error_string (const char *msg) {
  caml_raise_sys_error (caml_copy_string (msg));
}

static int n_events = sizeof (event_table) / sizeof (int);

static int __event_of_ml_event (value v) {
  int e = Int_val (v);
  if (e < n_events) return event_table [e];
  caml_raise_sys_error_string ("Unknown Papi.event constructor.");
}

static int n_errors = sizeof (error_table) / sizeof (int);

static int __error_of_ml_error (value v) {
  int e = Int_val (v);
  if (e < n_errors) return error_table [e];
  caml_raise_sys_error_string ("Unknown Papi.error constructor.");
}

static int __ml_error_of_error (int e) {
  for (int i = 0; i < n_errors; i++)
    if (error_table [i] == e) return Val_int (i);
  caml_raise_sys_error_string ("Unknown PAPI error.");
}

__attribute__ ((__noreturn__))
static void __raise_papi (int err, const char *f) {
  CAMLparam0();
  CAMLlocal1(arg);
  static value *exn = NULL;
  if (!exn) {
    exn = caml_named_value ("PAPI_EXCEPTION_CTOR");
    if (!exn) exit (1);
  }
  arg = caml_alloc_tuple (2);
  Field (arg, 0) = __ml_error_of_error (err);
  Field (arg, 1) = caml_copy_string (f);
  caml_raise_with_arg (*exn, arg);
}

static inline int __ret (int ret, const char *f) {
  if (ret < 0) __raise_papi (ret, f);
  return ret;
}

CAMLprim value caml_papi_library_init (__unit) {
  if (PAPI_is_initialized () == PAPI_NOT_INITED) {
    __ret (PAPI_library_init (PAPI_VER_CURRENT), "Papi.init");
    if (PAPI_is_initialized () == PAPI_NOT_INITED)
      caml_raise_sys_error_string ("Papi.init: cannot initialize PAPI");
  }
  return Val_unit;
}

CAMLprim value caml_papi_shutdown (__unit) {
  PAPI_shutdown ();
  return Val_unit;
}

CAMLprim value caml_papi_hw_counters (__unit) {
  return Val_int (__ret (PAPI_get_opt (PAPI_MAX_HWCTRS, NULL), "hw_counters"));
}

CAMLprim value caml_papi_strerror (value e) {
  if (PAPI_is_initialized () == PAPI_NOT_INITED)
    __raise_papi (PAPI_ENOINIT, "Papi.strerror");
  char *msg = PAPI_strerror (__error_of_ml_error (e));
  return caml_copy_string (msg ? msg : "");
}

#define EVENT_INFO_ACCESSOR(name, var, expr)                    \
  CAMLprim value caml_papi_event_ ## name (value e) {           \
    PAPI_event_info_t var = {0};                                \
    __ret (PAPI_get_event_info (__event_of_ml_event (e), &var), \
        "get_event_info");                                      \
    return (expr);                                              \
  }

EVENT_INFO_ACCESSOR(name, i,
  caml_copy_string (i.symbol + ((strncmp ("PAPI_", i.symbol, 5) == 0) ? 5 : 0)))
EVENT_INFO_ACCESSOR(descr, i, caml_copy_string (i.long_descr))
/* EVENT_INFO_ACCESSOR(short_descr, i, caml_copy_string (i.short_descr)) */

CAMLprim value caml_papi_create_eventset (__unit) {
  int es = PAPI_NULL;
  __ret (PAPI_create_eventset (&es), "Papi.create");
  return Val_int (es);
}

CAMLprim value caml_papi_cleanup_eventset (value es) {
  int i = Int_val (es);
  __ret (PAPI_cleanup_eventset (i), "Papi.cleanup");
  return Val_unit;
}

CAMLprim value caml_papi_destroy_eventset (value es) {
  int i = Int_val (es);
  __ret (PAPI_destroy_eventset (&i), "Papi.destroy");
  return Val_unit;
}

CAMLprim value caml_papi_add_event (value es, value event) {
  int res = PAPI_add_event (Int_val (es), __event_of_ml_event (event));
  __ret (res, "Papi.add");
  return Val_unit;
}

CAMLprim value caml_papi_remove_event (value es, value event) {
  int res = PAPI_remove_event (Int_val (es), __event_of_ml_event (event));
  __ret (res, "Papi.remove");
  return Val_unit;
}

CAMLprim value caml_papi_query_event (value event) {
  int res = PAPI_query_event (__event_of_ml_event (event));
  return (res == PAPI_OK ? Val_true : Val_false);
}

static inline int __ml_num_events (value es) {
  return __ret (PAPI_num_events (Int_val (es)), "Papi.num_events");
}

CAMLprim value caml_papi_num_events (value es) {
  return Val_int (__ml_num_events (es));
}

CAMLprim value caml_papi_start (value es) {
  __ret (PAPI_start (Int_val (es)), "Papi.start");
  return Val_unit;
}

CAMLprim value caml_papi_stop (value es) {
  __ret (PAPI_stop (Int_val (es), NULL), "Papi.stop");
  return Val_unit;
}

CAMLprim value caml_papi_reset (value es) {
  __ret (PAPI_reset (Int_val (es)), "Papi.reset");
  return Val_unit;
}

CAMLprim value caml_papi_read (value es, value off, value vs) {
  CAMLparam1 (vs);

  mlsize_t o    = __int_option (off, 0),
           size = caml_array_length (vs),
           n    = __ml_num_events (es);

  if (size <= o || size - o < n)
    caml_invalid_argument ("Papi.read");

  long long vals [n];
  __ret (PAPI_read (Int_val (es), vals), "Papi.read");

  for (mlsize_t i = 0; i < n; i++)
    Store_double_field (vs, i + o, (double) vals [i]);

  CAMLreturn (Val_unit);
}

CAMLprim value caml_papi_accum (value es, value off, value vs) {
  CAMLparam1 (vs);

  mlsize_t o = __int_option (off, 0),
           size = caml_array_length (vs),
           n = __ml_num_events (es);

  if (size <= o || size - o < n)
    caml_invalid_argument ("Papi.accum");

  long long vals [n];
  memset (vals, 0, n * sizeof (long long));
  __ret (PAPI_accum (Int_val (es), vals), "Papi.accum");

  for (mlsize_t i = 0; i < n; i++)
    Store_double_field (vs, i + o,
        Double_field (vs, i + o) + (double) vals [i]);

  CAMLreturn (Val_unit);
}
