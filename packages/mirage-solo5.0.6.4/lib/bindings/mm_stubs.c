/*
 * Copyright (c) 2019 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "solo5.h"

#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value
stub_mallinfo(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(res);
  struct mallinfo mi;

  mi = mallinfo();
  res = caml_alloc(7, 0);
  Store_field (res, 0, Val_int(mi.arena));
  Store_field (res, 1, Val_int(mi.ordblks));
  Store_field (res, 2, Val_int(mi.hblkhd));
  Store_field (res, 3, Val_int(mi.usmblks));
  Store_field (res, 4, Val_int(mi.uordblks));
  Store_field (res, 5, Val_int(mi.fordblks));
  Store_field (res, 6, Val_int(mi.keepcost));
  CAMLreturn(res);
}
