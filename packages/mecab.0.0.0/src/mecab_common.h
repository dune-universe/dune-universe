/* MeCab --- A MeCab binding for OCaml

   Copyright (c) 2017 Akinori ABE

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. */

#include <string.h>
#include <mecab.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

// `Type_val(v, T)` return a pointer of type `T` from a OCaml-style value `v`.
#define Type_val(v, type) (*((type **) Data_custom_val(v)))

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Mecab_val(v) Type_val(v, mecab_t)

static inline value
Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

static inline value
caml_copy_substring(const char *str, int len) {
  CAMLparam0();
  CAMLlocal1(substr);

  substr = caml_alloc_string(len);
  memcpy(String_val(substr), str, len);

  CAMLreturn(substr);
}

static value
caml_copy_custom(void * ptr, struct custom_operations * ops) {
  CAMLparam0();
  CAMLlocal1(ml_val);
  ml_val = caml_alloc_custom(ops, sizeof(void *), 0, 1);
  Type_val(ml_val, void) = ptr;
  CAMLreturn(ml_val);
}
