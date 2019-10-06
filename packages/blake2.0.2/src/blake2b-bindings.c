/*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff.
   Copyright (c) 2019 Vincent Botbol.
   All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>

#include "blake2.h"

CAMLprim value sizeof_blake2b_state(value unit) {
  return Val_int(sizeof(blake2b_state));
}

CAMLprim value blake2b_state_outlen(value S) {
  blake2b_state *s = (blake2b_state *)Bytes_val(S);
  return Val_int(s->outlen);
}

CAMLprim value ml_blake2b_init(value S, value outlen) {
  return Val_int(blake2b_init((blake2b_state *)Bytes_val(S), Int_val(outlen)));
}

CAMLprim value ml_blake2b_init_key(value S, value outlen, value key) {
  return Val_int(blake2b_init_key((blake2b_state *)Bytes_val(S),
                                  Int_val(outlen),
                                  (void *)Bytes_val(key),
                                  caml_string_length(key)));
}

CAMLprim value ml_blake2b_update(value S, value in) {
  return Val_int(blake2b_update((blake2b_state *)Bytes_val(S),
                                (void *)Bytes_val(in),
                                caml_string_length(in)));
}

CAMLprim value ml_blake2b_final(value S, value out) {
  return Val_int(blake2b_final((blake2b_state *)Bytes_val(S),
                               (void *)Bytes_val(out),
                               caml_string_length(out)));
}

CAMLprim value ml_blake2b(value out, value in, value key) {
  return Val_int(blake2b((void *)Bytes_val(out),
                         caml_string_length(out),
                         (void *)Bytes_val(in),
                         caml_string_length(in),
                         (void *)Bytes_val(key),
                         caml_string_length(key)));
}

/*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff
   Copyright (c) 2019 Vincent Botbol.

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
