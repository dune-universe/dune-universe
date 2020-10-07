#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <clang-c/Index.h>
#include "libclang_extensions.h"

struct tokens_closure {
  CXTranslationUnit tu;
  unsigned int count;
  CXToken *tokens;
};

static void
finalize_tokens_closure(value v) {
  struct tokens_closure *tokens_closure = Data_custom_val(v);
  clang_disposeTokens(tokens_closure->tu, tokens_closure->tokens,
    tokens_closure->count);;
}

struct custom_operations tokens_closure_ops = {
  "tokens_closure",
  finalize_tokens_closure,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

static value
new_tokens_closure(CXTranslationUnit tu, unsigned int count, CXToken *tokens)
{
  CAMLparam0();
  CAMLlocal1(tokens_closure_ocaml);
  tokens_closure_ocaml =
    caml_alloc_custom(&tokens_closure_ops, sizeof(struct tokens_closure), 0, 1);
  struct tokens_closure *tokens_closure = Data_custom_val(tokens_closure_ocaml);
  tokens_closure->tu = tu;
  tokens_closure->count = count;
  tokens_closure->tokens = tokens;
  CAMLreturn(tokens_closure_ocaml);
}

CXTranslationUnit Cxtranslationunit_val(value);
CXSourceLocation Cxsourcelocation_val(value);
CXSourceRange Cxsourcerange_val(value);
value Val_cxtoken(CXToken);

#ifndef LLVM_VERSION_BEFORE_7_0_0
CAMLprim value
clang_getToken_wrapper(value TU_ocaml, value Location_ocaml)
{
  CAMLparam2(TU_ocaml, Location_ocaml);
  CAMLlocal1(result_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  CXSourceLocation Location;
  Location = Cxsourcelocation_val(Field(Location_ocaml, 0));
  CXToken *result = clang_getToken(TU, Location);
  if (result == NULL) {
    result_ocaml = Val_int(0);
  }
  else {
    CAMLlocal2(tokens_closure_ocaml, tuple);
    tokens_closure_ocaml = new_tokens_closure(TU, 1, result);
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_cxtoken(*result));
    Store_field(tuple, 1, tokens_closure_ocaml);
    result_ocaml = caml_alloc(1, 0);
    Store_field(result_ocaml, 0, tuple);
  };
  CAMLreturn(result_ocaml);
}
#endif

CAMLprim value
clang_tokenize_wrapper(value TU_ocaml, value Range_ocaml)
{
  CAMLparam2(TU_ocaml, Range_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  CXSourceRange Range;
  Range = Cxsourcerange_val(Field(Range_ocaml, 0));
  unsigned int NumTokens;
  CXToken *Tokens;
  clang_tokenize(TU, Range, &Tokens, &NumTokens);
  CAMLlocal3(result_ocaml, tokens_closure_ocaml, tuple);
  tokens_closure_ocaml = new_tokens_closure(TU, NumTokens, Tokens);
  result_ocaml = caml_alloc(NumTokens, 0);
  for (unsigned int i = 0; i < NumTokens; i++) {
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, Val_cxtoken(Tokens[i]));
    Store_field(tuple, 1, tokens_closure_ocaml);
    Store_field(result_ocaml, i, tuple);
  }
  CAMLreturn(result_ocaml);
}
