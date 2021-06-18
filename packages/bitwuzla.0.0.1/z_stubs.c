#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <gmp.h>
#include <zarith.h>
#include <bzlacore.h>
#include <bzlabvstruct.h>
#include <bzlabv.h>
#include <bzlanode.h>

CAMLprim value
ocaml_bitwuzla_value_get_bv_bits (value vterm)
{
  BzlaNode *exp = (BzlaNode*) Long_val(vterm);
  Bzla *bzla = bzla_node_get_bzla(exp);
  exp = bzla_simplify_exp(bzla, exp);
  if (__builtin_expect(!bzla_node_is_bv_const(exp), 0)) {
    caml_invalid_argument("The term is not a bitvector value.");
    __builtin_unreachable();
  }
  BzlaBitVector *bits = bzla_node_bv_const_get_bits(exp);
  if (bits->width < GMP_LIMB_BITS)
    return Val_long(mpz_get_ui(bits->val));
  else
    return ml_z_from_mpz(bits->val);
}
