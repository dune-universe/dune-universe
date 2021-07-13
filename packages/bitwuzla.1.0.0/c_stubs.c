#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <bitwuzla.h>

static const char *formats[5] =
  { "btor", "btor2", "smt2", "aiger_ascii", "aiger_binary" };

static const value *vpp_print_string = NULL;

struct t { Bitwuzla *bitwuzla; value *termination_callback; };

#define Bitwuzla_val(v) (((struct t*)Data_custom_val(v))->bitwuzla)
#define TerminationCallback_val(v) \
  (((struct t*)Data_custom_val(v))->termination_callback)

#ifdef ARCH_SIXTYFOUR

static const char *pointer_conversion_error =
  "[Broken assumption] This ocaml binding greedily assumes the "
  "two most significant bits of the c pointers are always equal "
  "(and most likely cleared). Still, this error is the proof that "
  "the assumption may not always hold in practice. Please, file "
  "a bug to the package maintainer.";

#define Sort_val(v) ((BitwuzlaSort*)Long_val(v))
#define Term_val(v) ((BitwuzlaTerm*)Long_val(v))
#define Val_ptr(p)							\
  ({									\
    intnat __ptr = (intnat)p;						\
    if (__builtin_expect(__ptr != Long_val(Val_long(__ptr)), 0)) {	\
      caml_failwith(pointer_conversion_error);				\
      __builtin_unreachable();						\
    }									\
    Val_long(__ptr);							\
  })
#define Val_sort Val_ptr
#define Val_term Val_ptr

#define Val_array(t,c,p,s)						\
  ({									\
    int __len = (s);							\
    t *__buf = (p);							\
    value __val = caml_alloc(__len, 0);					\
    for (int __i; __i < __len; __i += 1)				\
      Field(__val, __i) = c(__buf[__i]);   /* assumes c do not alloc */ \
    __val;								\
  })

#else

#define Sort_val(v) ((BitwuzlaSort*)Nativeint_val(v))
#define Term_val(v) ((BitwuzlaTerm*)Nativeint_val(v))
#define Val_ptr(p) caml_copy_nativeint((intnat) p)
#define Val_sort Val_ptr
#define Val_term Val_ptr

#define Val_array(t,c,p,s)						\
  ({									\
    int __len = (s);							\
    t *__buf = (p);							\
    CAMLlocal1(__val);							\
    __val = caml_alloc(__len, 0);					\
    for (int __i; __i < __len; __i += 1)				\
      Store_field(__val, __i, c(__buf[__i]));				\
    __val;								\
  })

#endif

CAMLprim void
ocaml_bitwuzla_init (void)
{
  if (__builtin_expect(vpp_print_string == NULL, 1)) {
    vpp_print_string = caml_named_value("Format.pp_print_string");
    bitwuzla_set_abort_callback(caml_failwith);
  }
}


CAMLprim void
ocaml_bitwuzla_delete (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  value *termination_callback = TerminationCallback_val(vt);
  Bitwuzla_val(vt) = NULL;
  TerminationCallback_val(vt) = NULL;
  if (t != NULL) {
    bitwuzla_delete(t);
    if (termination_callback != NULL) {
      caml_remove_global_root(termination_callback);
      free(termination_callback);
    }
  }
}

static struct custom_operations bitwuzla_ops =
  {
   "https://bitwuzla.github.io",
   ocaml_bitwuzla_delete,
   custom_compare_default,
   custom_hash_default,
   custom_serialize_default,
   custom_deserialize_default,
   custom_compare_ext_default,
   custom_fixed_length_default
  };

CAMLprim value
ocaml_bitwuzla_new (void)
{
  Bitwuzla *t = bitwuzla_new();
  value vt = caml_alloc_custom(&bitwuzla_ops, sizeof(struct t), 0, 1);
  Bitwuzla_val(vt) = t;
  TerminationCallback_val(vt) = NULL;
  return vt;
}

CAMLprim void
ocaml_bitwuzla_reset (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  value *termination_callback = TerminationCallback_val(vt);
  TerminationCallback_val(vt) = NULL;
  if (termination_callback != NULL) {
    caml_remove_global_root(termination_callback);
    free(termination_callback);
  }
  bitwuzla_reset(t);
}

CAMLprim value
ocaml_bitwuzla_copyright (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return caml_copy_string(bitwuzla_copyright(t));
}

CAMLprim value
ocaml_bitwuzla_version (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return caml_copy_string(bitwuzla_version(t));
}

CAMLprim value
ocaml_bitwuzla_terminate(value vt)
{
   Bitwuzla *t =  Bitwuzla_val(vt);
   return Val_bool(bitwuzla_terminate(t));
}

static int32_t ocaml_callback (void *state)
{
  value *termination_callback = (value*)state;
  value vtc = *termination_callback;
  value vf = Field(vtc, 0);
  value va = Field(vtc, 1);
  return (int32_t) Long_val(caml_callback(vf, va));
}

CAMLprim value
ocaml_bitwuzla_set_termination_callback (value vt, value vtc)
{
  Bitwuzla *t =  Bitwuzla_val(vt);
  value *termination_callback = TerminationCallback_val(vt);
  if (termination_callback == NULL) {
    termination_callback = malloc(sizeof(value));
    TerminationCallback_val(vt) = termination_callback;
    *termination_callback = vtc;
    caml_register_global_root(termination_callback);
  } else {
    *termination_callback = vtc;
  }
  bitwuzla_set_termination_callback(t, ocaml_callback, termination_callback);
  return vt;
}

CAMLprim value
ocaml_bitwuzla_get_termination_callback_state (value vt)
{
  Bitwuzla *t =  Bitwuzla_val(vt);
  value *termination_callback = bitwuzla_get_termination_callback_state(t);
  value vtc = *termination_callback;
  return Field(vtc, 1);
}

CAMLprim void
native_bitwuzla_set_option (value vt, intnat opt, value vval)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t val = Long_val(vval);
  bitwuzla_set_option(t, opt, val);
}

CAMLprim void
ocaml_bitwuzla_set_option (value vt, value vopt, value vval)
{
  native_bitwuzla_set_option(vt, Long_val(vopt), vval);
}

CAMLprim void
native_bitwuzla_set_option_str (value vt, intnat opt, value vval)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  const char *val = String_val(vval);
  bitwuzla_set_option_str(t, opt, val);
}

CAMLprim void
ocaml_bitwuzla_set_option_str (value vt, value vopt, value vval)
{
  native_bitwuzla_set_option_str(vt, Long_val(vopt), vval);
}

CAMLprim value
native_bitwuzla_get_option (value vt, intnat opt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return Val_long(bitwuzla_get_option(t, opt));
}

CAMLprim value
ocaml_bitwuzla_get_option (value vt, value vopt)
{
  return native_bitwuzla_get_option(vt, Long_val(vopt));
}

CAMLprim value
native_bitwuzla_get_option_str (value vt, intnat opt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return caml_copy_string(bitwuzla_get_option_str(t, opt));
}

CAMLprim value
ocaml_bitwuzla_get_option_str (value vt, value vopt)
{
  return native_bitwuzla_get_option_str(vt, Long_val(vopt));
}

CAMLprim value
ocaml_bitwuzla_mk_array_sort (value vt, value vi, value ve)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *index = Sort_val(vi);
  BitwuzlaSort *element = Sort_val(ve);
  return Val_sort(bitwuzla_mk_array_sort(t, index, element));
}

CAMLprim value
ocaml_bitwuzla_mk_bool_sort (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return Val_sort(bitwuzla_mk_bool_sort(t));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_sort (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t size = Long_val(vs);
  return Val_sort(bitwuzla_mk_bv_sort(t, size));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_sort (value vt, value ves, value vss)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t exp_size = Long_val(ves);
  uint32_t sig_size = Long_val(vss);
  return Val_sort(bitwuzla_mk_fp_sort(t, exp_size, sig_size));
}

CAMLprim value
ocaml_bitwuzla_mk_fun_sort (value vt, value vd, value vc)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t arity = Wosize_val(vd);
  BitwuzlaSort **domain = alloca(sizeof(BitwuzlaSort*) * arity);
  for (int i = 0; i < arity; i += 1)
    domain[i] = Sort_val(Field(vd, i));
  BitwuzlaSort *codomain = Sort_val(vc);
  return Val_sort(bitwuzla_mk_fun_sort(t, arity, domain, codomain));
}

CAMLprim value
ocaml_bitwuzla_mk_rm_sort (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return Val_sort(bitwuzla_mk_rm_sort(t));
}

CAMLprim value
ocaml_bitwuzla_mk_true (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return Val_term(bitwuzla_mk_true(t));
}

CAMLprim value
ocaml_bitwuzla_mk_false (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return Val_term(bitwuzla_mk_false(t));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_zero (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_bv_zero(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_one (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_bv_one(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_ones (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_bv_ones(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_min_signed (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_bv_min_signed(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_max_signed (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_bv_max_signed(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_pos_zero (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_fp_pos_zero(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_neg_zero (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_fp_neg_zero(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_pos_inf (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_fp_pos_inf(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_neg_inf (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_fp_neg_inf(t, sort));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_nan (value vt, value vs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_term(bitwuzla_mk_fp_nan(t, sort));
}

CAMLprim value
native_bitwuzla_mk_bv_value (value vt, value vs, value vv, intnat b)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  const char *val = String_val(vv);
  BitwuzlaBVBase base = b;
  return Val_term(bitwuzla_mk_bv_value(t, sort, val, base));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_value (value vt, value vs, value vv, value vb)
{
  return native_bitwuzla_mk_bv_value(vt, vs, vv, Long_val(vb));
}

CAMLprim value
ocaml_bitwuzla_mk_bv_value_int (value vt, value vs, value vv)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  uint64_t val = Long_val(vv);
  return Val_term(bitwuzla_mk_bv_value_uint64(t, sort, val));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_value (value vt, value vs, value vbe, value vbs)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *sign = Term_val(vs);
  BitwuzlaTerm *exponent = Term_val(vbe);
  BitwuzlaTerm *significand = Term_val(vbs);
  return Val_term(bitwuzla_mk_fp_value(t, sign, exponent, significand));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_value_from_real (value vt, value vs, value vr, value vn)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  BitwuzlaTerm *rm = Term_val(vr);
  const char *real = String_val(vn);
  return Val_term(bitwuzla_mk_fp_value_from_real(t, sort, rm, real));
}

CAMLprim value
ocaml_bitwuzla_mk_fp_value_from_rational
(value vt, value vs, value vr, value vn, value vd)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  BitwuzlaTerm *rm = Term_val(vr);
  const char *num = String_val(vn);
  const char *den = String_val(vd);
  return Val_term(bitwuzla_mk_fp_value_from_rational(t, sort, rm, num, den));
}

CAMLprim value
native_bitwuzla_mk_rm_value (value vt, intnat r)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaRoundingMode rm = r;
  return Val_term(bitwuzla_mk_rm_value(t, rm));
}

CAMLprim value
ocaml_bitwuzla_mk_rm_value (value vt, value vr)
{
  return native_bitwuzla_mk_rm_value(vt, Long_val(vr));
}

CAMLprim value
native_bitwuzla_mk_term1 (value vt, intnat k, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg = Term_val(va);
  return Val_term(bitwuzla_mk_term1(t, kind, arg));
}

CAMLprim value
ocaml_bitwuzla_mk_term1 (value vt, value vk, value va)
{
  return native_bitwuzla_mk_term1(vt, Long_val(vk), va);
}

CAMLprim value
native_bitwuzla_mk_term2 (value vt, intnat k, value va0, value va1)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg0 = Term_val(va0);
  const BitwuzlaTerm *arg1 = Term_val(va1);
  return Val_term(bitwuzla_mk_term2(t, kind, arg0, arg1));
}

CAMLprim value
ocaml_bitwuzla_mk_term2 (value vt, value vk, value va0, value va1)
{
  return native_bitwuzla_mk_term2(vt, Long_val(vk), va0, va1);
}

CAMLprim value
native_bitwuzla_mk_term3 (value vt, intnat k, value va0, value va1, value va2)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg0 = Term_val(va0);
  const BitwuzlaTerm *arg1 = Term_val(va1);
  const BitwuzlaTerm *arg2 = Term_val(va2);
  return Val_term(bitwuzla_mk_term3(t, kind, arg0, arg1, arg2));
}

CAMLprim value
ocaml_bitwuzla_mk_term3 (value vt, value vk, value va0, value va1, value va2)
{
  return native_bitwuzla_mk_term3(vt, Long_val(vk), va0, va1, va2);
}

CAMLprim value
native_bitwuzla_mk_term (value vt, intnat k, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  uint32_t argc = Wosize_val(va);
  BitwuzlaTerm **args = alloca(sizeof(BitwuzlaTerm*) * argc);
  for (int i = 0; i < argc; i += 1)
    args[i] = Term_val(Field(va, i));
  return Val_term(bitwuzla_mk_term(t, kind, argc, args));
}

CAMLprim value
ocaml_bitwuzla_mk_term (value vt, value vk, value va)
{
  return native_bitwuzla_mk_term(vt, Long_val(vk), va);
}

CAMLprim value
native_bitwuzla_mk_term1_indexed1 (value vt, intnat k, value va, value vi)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg = Term_val(va);
  uint32_t idx = Long_val(vi);
  return Val_term(bitwuzla_mk_term1_indexed1(t, kind, arg, idx));
}

CAMLprim value
ocaml_bitwuzla_mk_term1_indexed1 (value vt, value vk, value va, value vi)
{
  return native_bitwuzla_mk_term1_indexed1(vt, Long_val(vk), va, vi);
}

CAMLprim value
native_bitwuzla_mk_term1_indexed2
(value vt, intnat k, value va, value vi0, value vi1)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg = Term_val(va);
  uint32_t idx0 = Long_val(vi0);
  uint32_t idx1 = Long_val(vi1);
  return Val_term(bitwuzla_mk_term1_indexed2(t, kind, arg, idx0, idx1));
}

CAMLprim value
ocaml_bitwuzla_mk_term1_indexed2
(value vt, value vk, value va, value vi0, value vi1)
{
  return native_bitwuzla_mk_term1_indexed2(vt, Long_val(vk), va, vi0, vi1);
}

CAMLprim value
native_bitwuzla_mk_term2_indexed1
(value vt, intnat k, value va0, value va1, value vi)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg0 = Term_val(va0);
  const BitwuzlaTerm *arg1 = Term_val(va1);
  uint32_t idx = Long_val(vi);
  return Val_term(bitwuzla_mk_term2_indexed1(t, kind, arg0, arg1, idx));
}

CAMLprim value
ocaml_bitwuzla_mk_term2_indexed1
(value vt, value vk, value va0, value va1, value vi)
{
  return native_bitwuzla_mk_term2_indexed1(vt, Long_val(vk), va0, va1, vi);
}

CAMLprim value
native_bitwuzla_mk_term2_indexed2
(value vt, intnat k, value va0, value va1, value vi0, value vi1)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  const BitwuzlaTerm *arg0 = Term_val(va0);
  const BitwuzlaTerm *arg1 = Term_val(va1);
  uint32_t idx0 = Long_val(vi0);
  uint32_t idx1 = Long_val(vi1);
  return Val_term(bitwuzla_mk_term2_indexed2(t, kind, arg0, arg1, idx0, idx1));
}

CAMLprim value
ocaml_bitwuzla_mk_term2_indexed2
(value vt, value vk, value va0, value va1, value vi0, value vi1)
{
  return native_bitwuzla_mk_term2_indexed2(vt, Long_val(vk),
					   va0, va1, vi0, vi1);
}

CAMLprim value
ocaml_bitwuzla_mk_term2_indexed2_byte6 (value argv[], int argc)
{
  return ocaml_bitwuzla_mk_term2_indexed2(argv[0], argv[1], argv[2],
					  argv[3], argv[4], argv[5]);
}

CAMLprim value
native_bitwuzla_mk_term_indexed (value vt, intnat k, value va, value vi)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaKind kind = k;
  uint32_t argc = Wosize_val(va);
  BitwuzlaTerm **args = alloca(sizeof(BitwuzlaTerm*) * argc);
  for (int i = 0; i < argc; i += 1)
    args[i] = Term_val(Field(va, i));
  uint32_t idxc = Wosize_val(vi);
  uint32_t *idxs = alloca(sizeof(uint32_t) * idxc);
  for (int i = 0; i < idxc; i += 1)
    idxs[i] = Long_val(Field(vi, i));
  return Val_term(bitwuzla_mk_term_indexed(t, kind, argc, args, idxc, idxs));
}

CAMLprim value
ocaml_bitwuzla_mk_term_indexed (value vt, value vk, value va, value vi)
{
  return native_bitwuzla_mk_term_indexed(vt, Long_val(vk), va, vi);
}

CAMLprim value
ocaml_bitwuzla_mk_const (value vt, value vs, value vn)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  const char *symbol = caml_string_length(vn) ? String_val(vn) : NULL;
  return Val_term(bitwuzla_mk_const(t, sort, symbol));
}

CAMLprim value
ocaml_bitwuzla_mk_const_array (value vt, value vs, value vv)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  BitwuzlaTerm *val = Term_val(vv);
  return Val_term(bitwuzla_mk_const_array(t, sort, val));
}

CAMLprim value
ocaml_bitwuzla_mk_var (value vt, value vs, value vn)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaSort *sort = Sort_val(vs);
  const char *symbol = caml_string_length(vn) ? String_val(vn) : NULL;
  return Val_term(bitwuzla_mk_var(t, sort, symbol));
}

CAMLprim void
ocaml_bitwuzla_push (value vt, value vn)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t nlevels = Long_val(vn);
  bitwuzla_push(t, nlevels);
}

CAMLprim void
ocaml_bitwuzla_pop (value vt, value vn)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  uint32_t nlevels = Long_val(vn);
  bitwuzla_pop(t, nlevels);
}

CAMLprim void
ocaml_bitwuzla_assert (value vt, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  const BitwuzlaTerm *term = Term_val(va);
  bitwuzla_assert(t, term);
}

CAMLprim void
ocaml_bitwuzla_assume (value vt, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  const BitwuzlaTerm *term = Term_val(va);
  bitwuzla_assume(t, term);
}

CAMLprim value
ocaml_bitwuzla_is_unsat_assumption (value vt, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  const BitwuzlaTerm *term = Term_val(va);
  return Val_bool(bitwuzla_is_unsat_assumption(t, term));
}

CAMLprim value
ocaml_bitwuzla_get_unsat_assumptions (value vt)
{
  CAMLparam0();
  Bitwuzla *t = Bitwuzla_val(vt);
  size_t size;
  BitwuzlaTerm **ptr = bitwuzla_get_unsat_assumptions(t, &size);
  CAMLreturn(Val_array(BitwuzlaTerm*, Val_term, ptr, size));
}

CAMLprim value
ocaml_bitwuzla_get_unsat_core (value vt)
{
  CAMLparam0();
  Bitwuzla *t = Bitwuzla_val(vt);
  size_t size;
  BitwuzlaTerm **ptr = bitwuzla_get_unsat_core(t, &size);
  CAMLreturn(Val_array(BitwuzlaTerm*, Val_term, ptr, size));
}

CAMLprim void
ocaml_bitwuzla_fixate_assumptions (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  bitwuzla_fixate_assumptions(t);
}

CAMLprim void
ocaml_bitwuzla_reset_assumptions (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  bitwuzla_reset_assumptions(t);
}

CAMLprim intnat
native_bitwuzla_simplify (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return bitwuzla_simplify(t);
}

CAMLprim value
ocaml_bitwuzla_simplify (value vt)
{
  return Val_long(native_bitwuzla_simplify(vt));
}

CAMLprim intnat
native_bitwuzla_check_sat (value vt)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  return bitwuzla_check_sat(t);
}

CAMLprim value
ocaml_bitwuzla_check_sat (value vt)
{
  return Val_long(native_bitwuzla_check_sat(vt));
}

CAMLprim value
ocaml_bitwuzla_get_value (value vt, value va)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(va);
  return Val_term(bitwuzla_get_value(t, term));
}

static ssize_t pp_out_string (void *c, const char *buf, size_t size)
{
  value vstr = caml_alloc_initialized_string(size, buf);
  value *vformatter = (value*) c;
  caml_callback2(*vpp_print_string, *vformatter, vstr);
  return size;
}

static cookie_io_functions_t formatter_cookie =
  { NULL, pp_out_string, NULL, NULL };

static FILE *openfilefromformatter (value *vformatter)
{
  FILE *file = fopencookie((void*)vformatter, "w", formatter_cookie);
  setbuf(file, NULL);
  return file;
}

CAMLprim value
ocaml_bitwuzla_get_bv_value (value vt, value vterm)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(vterm);
  return caml_copy_string(bitwuzla_get_bv_value(t, term));
}

CAMLprim value
ocaml_bitwuzla_get_fp_value (value vt, value vterm)
{
  CAMLparam0();
  CAMLlocal3(vsign, vexponent, vsignificand);
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(vterm);
  const char *sign, *exponent, *significand;
  bitwuzla_get_fp_value(t, term, &sign, &exponent, &significand);
  vsign = caml_copy_string(sign);
  vexponent = caml_copy_string(exponent);
  vsignificand = caml_copy_string(significand);
  value vtuple = caml_alloc_small(3, 0);
  Field(vtuple, 0) = vsign;
  Field(vtuple, 1) = vexponent;
  Field(vtuple, 2) = vsignificand;
  CAMLreturn(vtuple);
}

CAMLprim value
ocaml_bitwuzla_get_rm_value(value vt, value vterm)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(vterm);
  return caml_copy_string(bitwuzla_get_rm_value(t, term));
}

CAMLprim value
ocaml_bitwuzla_get_array_value (value vt, value vterm)
{
  CAMLparam0();
  CAMLlocal3(vassoc, vtuple, vdefault);
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(vterm);
  BitwuzlaTerm **indices, **values, *default_value;
  size_t size;
  bitwuzla_get_array_value(t, term, &indices, &values, &size, &default_value);
  vassoc = Atom(0);
  if (size > 0) {
    vassoc = caml_alloc(size, 0);
    for (int i = 0; i < size; i += 1) {
      vtuple = caml_alloc_small(2, 0);
      Store_field(vtuple, 0, Val_term(indices[i]));
      Store_field(vtuple, 1, Val_term(values[i]));
      Store_field(vassoc, i, vtuple);
    }
  }
  if (default_value != NULL) {
    vdefault = caml_alloc_small(1, 0);
    Store_field(vdefault, 0, Val_term(default_value));;
  }
  vtuple = caml_alloc_small(2, 0);
  Field(vtuple, 0) = vassoc;
  Field(vtuple, 1) = vdefault;
  CAMLreturn(vtuple);
}

CAMLprim value
ocaml_bitwuzla_get_fun_value (value vt, value vterm)
{
  CAMLparam0();
  CAMLlocal2(vassoc, vtuple);
  Bitwuzla *t = Bitwuzla_val(vt);
  BitwuzlaTerm *term = Term_val(vterm);
  BitwuzlaTerm ***args, **values;
  size_t arity, size;
  bitwuzla_get_fun_value(t, term, &args, &arity, &values, &size);
  vassoc = Atom(0);
  if (size > 0) {
    vassoc = caml_alloc(size, 0);
    for (int i = 0; i < size; i += 1) {
      vtuple = caml_alloc(arity + 1, 0);
      for (int j = 0; j < arity; j += 1) {
	Store_field(vtuple, j, Val_term(args[i][j]));
      }
      Store_field(vtuple, arity, Val_term(values[i]));
      Store_field(vassoc, i, vtuple);
    }
  }
  CAMLreturn(vassoc);
}

CAMLprim void
native_bitwuzla_print_model (value vt, intnat iformat, value vformatter)
{
  CAMLparam1(vformatter);
  Bitwuzla *t = Bitwuzla_val(vt);
  const char *format = formats[iformat];
  FILE *file = openfilefromformatter(&vformatter);
  bitwuzla_print_model(t, format, file);
  fclose(file);
  CAMLreturn0;
}

CAMLprim void
ocaml_bitwuzla_print_model (value vt, value viformat, value vformatter)
{
  native_bitwuzla_print_model(vt, Long_val(viformat), vformatter);
}

CAMLprim void
native_bitwuzla_dump_formula (value vt, intnat iformat, value vformatter)
{
  CAMLparam1(vformatter);
  Bitwuzla *t = Bitwuzla_val(vt);
  const char *format = formats[iformat];
  FILE *file = openfilefromformatter(&vformatter);
  bitwuzla_dump_formula(t, format, file);
  fclose(file);
  CAMLreturn0;
}

CAMLprim void
ocaml_bitwuzla_dump_formula (value vt, value viformat, value vformatter)
{
  native_bitwuzla_dump_formula(vt, Long_val(viformat), vformatter);
}

CAMLprim intnat
native_bitwuzla_parse (value vt, value vpath, value vformatter)
{
  CAMLparam1(vformatter);
  Bitwuzla *t = Bitwuzla_val(vt);
  char *path = strdup(String_val(vpath));
  const char *name = basename(path);
  FILE *infile = fopen(path, "r");
  FILE *outfile = openfilefromformatter(&vformatter);
  char *error = NULL;
  BitwuzlaResult status;
  bool smt2;
  BitwuzlaResult result =
    bitwuzla_parse(t, infile, name, outfile, &error, &status, &smt2);
  free(path);
  fclose(infile);
  fclose(outfile);
  if (error) {
    caml_failwith(error);
    __builtin_unreachable();
  }
  CAMLreturn(result);
}

CAMLprim value
ocaml_bitwuzla_parse (value vt, value vpath, value vformatter)
{
  return Val_long(native_bitwuzla_parse(vt, vpath, vformatter));
}

CAMLprim intnat
native_bitwuzla_parse_format
(value vt, intnat iformat, value vpath, value vformatter)
{
  CAMLparam1(vformatter);
  Bitwuzla *t = Bitwuzla_val(vt);
  const char *format = formats[iformat];
  char *path = strdup(String_val(vpath));
  const char *name = basename(path);
  FILE *infile = fopen(path, "r");
  FILE *outfile = openfilefromformatter(&vformatter);
  char *error = NULL;
  BitwuzlaResult status;
  bool smt2;
  BitwuzlaResult result =
    bitwuzla_parse_format(t, format, infile, name, outfile, &error, &status);
  free(path);
  fclose(infile);
  fclose(outfile);
  if (error) {
    caml_failwith(error);
    __builtin_unreachable();
  }
  CAMLreturn(result);
}

CAMLprim value
ocaml_bitwuzla_parse_format
(value vt, value viformat, value vpath, value vformatter)
{
  return Val_long(native_bitwuzla_parse_format(vt, Long_val(viformat),
					       vpath, vformatter));
}

CAMLprim value
ocaml_bitwuzla_substitute_term (value vt, value va, value vb)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  const BitwuzlaTerm *term = Term_val(va);
  size_t size = Wosize_val(vb);
  BitwuzlaTerm **keys = alloca(sizeof(BitwuzlaTerm*) * size);
  BitwuzlaTerm **values = alloca(sizeof(BitwuzlaTerm*) * size);
  for (int i = 0; i < size; i += 1) {
    value vbind = Field(vb, i);
    keys[i] = Term_val(Field(vbind, 0));
    values[i] = Term_val(Field(vbind, 1));
  }
  return Val_term(bitwuzla_substitute_term(t, term, size, keys, values));
}

CAMLprim void
ocaml_bitwuzla_substitute_terms (value vt, value va, value vb)
{
  Bitwuzla *t = Bitwuzla_val(vt);
  size_t argc = Wosize_val(va);
  BitwuzlaTerm **argv = alloca(sizeof(BitwuzlaTerm*) * argc);
  for (int i = 0; i < argc; i += 1)
    argv[i] = Term_val(Field(va, i));
  size_t size = Wosize_val(vb);
  BitwuzlaTerm **keys = alloca(sizeof(BitwuzlaTerm*) * size);
  BitwuzlaTerm **values = alloca(sizeof(BitwuzlaTerm*) * size);
  for (int i = 0; i < size; i += 1) {
    value vbind = Field(vb, i);
    keys[i] = Term_val(Field(vbind, 0));
    values[i] = Term_val(Field(vbind, 1));
  }
  bitwuzla_substitute_terms(t, argc, argv, size, keys, values);
  for (int i = 0; i < argc; i += 1) {
    Field(va, i) = Val_term(argv[i]);
  }
}

CAMLprim value
ocaml_bitwuzla_sort_hash (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_long(bitwuzla_sort_hash(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_bv_get_size (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_long(bitwuzla_sort_bv_get_size(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_fp_get_exp_size (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_long(bitwuzla_sort_fp_get_exp_size(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_fp_get_sig_size (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_long(bitwuzla_sort_fp_get_sig_size(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_array_get_index (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_sort(bitwuzla_sort_array_get_index(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_array_get_element (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_sort(bitwuzla_sort_array_get_element(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_fun_get_domain_sorts (value vs)
{
  CAMLparam0();
  BitwuzlaSort *sort = Sort_val(vs);
  size_t size;
  BitwuzlaSort **ptr = bitwuzla_sort_fun_get_domain_sorts(sort, &size);
  CAMLreturn(Val_array(BitwuzlaSort*, Val_sort, ptr, size));
}

CAMLprim value
ocaml_bitwuzla_sort_fun_get_codomain (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_sort(bitwuzla_sort_fun_get_codomain(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_fun_get_arity (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_long(bitwuzla_sort_fun_get_arity(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_is_equal (value vs0, value vs1)
{
  BitwuzlaSort *sort0 = Sort_val(vs0);
  BitwuzlaSort *sort1 = Sort_val(vs1);
  return Val_bool(bitwuzla_sort_is_equal(sort0, sort1));
}

CAMLprim value
ocaml_bitwuzla_sort_is_array (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_bool(bitwuzla_sort_is_array(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_is_bv (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_bool(bitwuzla_sort_is_bv(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_is_fp (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_bool(bitwuzla_sort_is_fp(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_is_fun (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_bool(bitwuzla_sort_is_fun(sort));
}

CAMLprim value
ocaml_bitwuzla_sort_is_rm (value vs)
{
  BitwuzlaSort *sort = Sort_val(vs);
  return Val_bool(bitwuzla_sort_is_rm(sort));
}

CAMLprim void
native_bitwuzla_sort_dump (value vsort, intnat iformat, value vformatter)
{
  CAMLparam1(vformatter);
  BitwuzlaSort *sort = Sort_val(vsort);
  const char *format = formats[iformat];
  FILE *file = openfilefromformatter(&vformatter);
  bitwuzla_sort_dump(sort, format, file);
  fclose(file);
  CAMLreturn0;
}

CAMLprim void
ocaml_bitwuzla_sort_dump (value vsort, value viformat, value vformatter)
{
  native_bitwuzla_sort_dump(vsort, Long_val(viformat), vformatter);
}

CAMLprim value
ocaml_bitwuzla_term_hash (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_long(bitwuzla_term_hash(term));
}

CAMLprim intnat
native_bitwuzla_term_get_kind (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return bitwuzla_term_get_kind(term);
}

CAMLprim value
ocaml_bitwuzla_term_get_kind (value ve)
{
  return Val_long(ocaml_bitwuzla_term_get_kind(ve));
}

CAMLprim value
ocaml_bitwuzla_term_get_children (value ve)
{
  CAMLparam0();
  BitwuzlaTerm *term = Term_val(ve);
  size_t size;
  BitwuzlaTerm **ptr = bitwuzla_term_get_children(term, &size);
  if (ptr == NULL) return Atom(0);
  CAMLreturn(Val_array(BitwuzlaTerm*, Val_term, ptr, size));
}

CAMLprim value
ocaml_bitwuzla_term_get_indices (value ve)
{
  CAMLparam0();
  BitwuzlaTerm *term = Term_val(ve);
  size_t size;
  uint32_t *ptr = bitwuzla_term_get_indices(term, &size);
  if (ptr == NULL) return Atom(0);
  CAMLreturn(Val_array(uint32_t, Val_long, ptr, size));
}

CAMLprim value
ocaml_bitwuzla_term_is_indexed (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_indexed(term));
}

CAMLprim value
ocaml_bitwuzla_term_get_sort (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_sort(bitwuzla_term_get_sort(term));
}

CAMLprim value
ocaml_bitwuzla_term_array_get_index_sort (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_sort(bitwuzla_term_array_get_index_sort(term));
}

CAMLprim value
ocaml_bitwuzla_term_array_get_element_sort (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_sort(bitwuzla_term_array_get_element_sort(term));
}

CAMLprim value
ocaml_bitwuzla_term_fun_get_domain_sorts (value ve)
{
  CAMLparam0();
  BitwuzlaTerm *term = Term_val(ve);
  size_t size;
  BitwuzlaSort **ptr = bitwuzla_term_fun_get_domain_sorts(term, &size);
  CAMLreturn(Val_array(BitwuzlaSort*, Val_sort, ptr, size));
}

CAMLprim value
ocaml_bitwuzla_term_fun_get_codomain_sort (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_sort(bitwuzla_term_fun_get_codomain_sort(term));
}

CAMLprim value
ocaml_bitwuzla_term_bv_get_size (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_long(bitwuzla_term_bv_get_size(term));
}

CAMLprim value
ocaml_bitwuzla_term_fp_get_exp_size (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_long(bitwuzla_term_fp_get_exp_size(term));
}

CAMLprim value
ocaml_bitwuzla_term_fp_get_sig_size (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_long(bitwuzla_term_fp_get_sig_size(term));
}

CAMLprim value
ocaml_bitwuzla_term_fun_get_arity (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_long(bitwuzla_term_fun_get_arity(term));
}

CAMLprim value
ocaml_bitwuzla_term_get_symbol (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  const char *symbol = bitwuzla_term_get_symbol(term);
  if (symbol == NULL) {
    caml_raise_not_found();
    __builtin_unreachable();
  }
  return caml_copy_string(symbol);
}

CAMLprim void
ocaml_bitwuzla_term_set_symbol (value ve, value vn)
{
  BitwuzlaTerm *term = Term_val(ve);
  const char *symbol = String_val(vn);
  bitwuzla_term_set_symbol(term, symbol);
}

CAMLprim value
ocaml_bitwuzla_term_is_equal_sort (value ve0, value ve1)
{
  BitwuzlaTerm *term0 = Term_val(ve0);
  BitwuzlaTerm *term1 = Term_val(ve1);
  return Val_bool(bitwuzla_term_is_equal_sort(term0, term1));
}

CAMLprim value
ocaml_bitwuzla_term_is_array (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_array(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_const (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_const(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fun (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fun(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_var (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_var(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bound_var (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bound_var(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_value (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_value(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_rm_value (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_rm_value(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_rm (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_rm(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value_zero (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value_zero(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value_one (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value_one(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value_ones (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value_ones(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value_min_signed (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value_min_signed(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_bv_value_max_signed (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_bv_value_max_signed(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value_pos_zero (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value_pos_zero(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value_neg_zero (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value_neg_zero(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value_pos_inf (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value_pos_inf(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value_neg_inf (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value_neg_inf(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_fp_value_nan (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_fp_value_nan(term));
}

CAMLprim value
ocaml_bitwuzla_term_is_const_array (value ve)
{
  BitwuzlaTerm *term = Term_val(ve);
  return Val_bool(bitwuzla_term_is_const_array(term));
}

CAMLprim void
native_bitwuzla_term_dump (value vterm, intnat iformat, value vformatter)
{
  CAMLparam1(vformatter);
  BitwuzlaTerm *term = Term_val(vterm);
  const char *format = formats[iformat];
  FILE *file = openfilefromformatter(&vformatter);
  bitwuzla_term_dump(term, format, file);
  fclose(file);
  CAMLreturn0;
}

CAMLprim void
ocaml_bitwuzla_term_dump (value vterm, value viformat, value vformatter)
{
  native_bitwuzla_term_dump(vterm, Long_val(viformat), vformatter);
}
