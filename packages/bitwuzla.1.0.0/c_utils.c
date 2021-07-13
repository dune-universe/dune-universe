#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <gmp.h>
#include <zarith.h>
#include <bzlacore.h>
#include "bzlaexp.h"
#include <bzlabvstruct.h>
#include <bzlabv.h>
#include <bzlanode.h>

#ifdef ARCH_SIXTYFOUR

#define Node_val(v) ((BzlaNode*)Long_val(v))
#define mpz_get_ulli mpz_get_ui

#else

#define Node_val(v) ((BzlaNode*)Nativeint_val(v))
#define mpz_get_ulli(z)							\
  ({									\
    unsigned long long int __r;						\
    mpz_export(&__r, 0, -1, sizeof(unsigned long long int), 0, 0, (z));	\
    __r;								\
  })

#endif


CAMLprim value
ocaml_bitwuzla_value_get_bv_bits (value vterm)
{
  BzlaNode *exp = Node_val(vterm);
  Bzla *bzla = bzla_node_get_bzla(exp);
  exp = bzla_simplify_exp(bzla, exp);
  BzlaBitVector *bits = bzla_node_bv_const_get_bits(exp);
  if (bits->width < GMP_LIMB_BITS)
    return Val_long(mpz_get_ui(bits->val));
  else
    return ml_z_from_mpz(bits->val);
}

typedef union {
  double d;
  unsigned long long int b;
} ieee754;

CAMLprim value
ocaml_bitwuzla_value_get_fp (value vrm, value vterm)
{
  BzlaNode *rm = Node_val(vrm);
  BzlaNode *exp = Node_val(vterm);
  Bzla *bzla = bzla_node_get_bzla(exp);
  BzlaSortId sort = bzla_sort_fp(bzla, 11, 53);
  BzlaNode *res = bzla_exp_fp_to_fp_from_fp(bzla, rm, exp, sort);
  bzla_sort_release(bzla, sort);
  res = bzla_simplify_exp(bzla, res);
  BzlaBitVector *bv = bzla_fp_as_bv(bzla, bzla_node_fp_const_get_fp(res));
  ieee754 val;
  val.b = mpz_get_ulli(bv->val);
  return caml_copy_double(val.d);
}
