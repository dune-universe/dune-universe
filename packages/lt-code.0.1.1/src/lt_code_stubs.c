#include <string.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim void xor_into_stub (value src_, value into_) {
  int len = Caml_ba_array_val(src_)->dim[0];
  const char* src = (const char*) Caml_ba_data_val(src_);
  char* into = (char*) Caml_ba_data_val(into_);
  for (int i = 0; i < len; i++) {
    into[i] ^= src[i];
  }
}

CAMLprim void memcpy_stub (value src_, value dst_) {
  int len = Caml_ba_array_val(src_)->dim[0];
  const char* src = (const char*) Caml_ba_data_val(src_);
  char* dst = (char*) Caml_ba_data_val(dst_);
  memcpy(dst, src, len);
}
