#include "zxcvbn.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value caml_match_type_val(ZxcTypeMatch_t type) {
  CAMLparam0();
  CAMLlocal1(caml_match_type);
  int tag;

  tag = (int)type % 32;
  if(tag <= 10) {
    caml_match_type = Val_int(tag);
  } else {
    caml_match_type = caml_alloc(1, 0);
    Store_field(caml_match_type, 0, Val_int(tag));
  }

  CAMLreturn(caml_match_type);
}

value caml_match_val(ZxcMatch_t match) {
  CAMLparam0();
  CAMLlocal1(caml_match);

  caml_match = caml_alloc(6, 0);
  Store_field(caml_match, 0, Val_int(match.Begin));
  Store_field(caml_match, 1, Val_int(match.Length));
  Store_field(caml_match, 2, caml_copy_double(match.Entrpy));
  Store_field(caml_match, 3, caml_match_type_val(match.Type));
  Store_field(caml_match, 4, Val_bool((int)match.Type >= 32));
  Store_field(caml_match, 5, caml_copy_double(match.MltEnpy));
  CAMLreturn(caml_match);
}

value caml_match_list_val(ZxcMatch_t *matches) {
  CAMLparam0();
  CAMLlocal3(caml_match_list, caml_match, tail);
  ZxcMatch_t *m = matches;

  caml_match_list = Val_int(0);
  while(m) {
    caml_match = caml_match_val(*m);
    tail = caml_match_list;
    caml_match_list = caml_alloc(2, 0);
    Store_field(caml_match_list, 0, caml_match);
    Store_field(caml_match_list, 1, tail);
    m = m->Next;
  }

  CAMLreturn(caml_match_list);
}

CAMLprim value zxcvbn_match_caml(value pwd) {
  CAMLparam1(pwd);
  CAMLlocal3(caml_matches, result, score);
  ZxcMatch_t *matches;

  score = caml_copy_double(ZxcvbnMatch(String_val(pwd), NULL, &matches));
  caml_matches = caml_match_list_val(matches);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, score);
  Store_field(result, 1, caml_matches);

  ZxcvbnFreeInfo(matches);
  CAMLreturn(result);
}
