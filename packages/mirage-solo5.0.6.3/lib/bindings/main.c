/*
 * Copyright (c) 2019 Martin Lucina <martin@lucina.net>
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

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>

static char *unused_argv[] = { "mirage", NULL };
static const char *solo5_cmdline = "";

CAMLprim value
mirage_solo5_yield_2(value v_deadline)
{
    CAMLparam1(v_deadline);

    solo5_time_t deadline = (Int64_val(v_deadline));
    solo5_handle_set_t ready_set;
    solo5_yield(deadline, &ready_set);

    CAMLreturn(caml_copy_int64(ready_set));
}

CAMLprim value
mirage_solo5_get_cmdline(value unit)
{
    CAMLparam1(unit);

    CAMLreturn(caml_copy_string(solo5_cmdline));
}

extern void _nolibc_init(uintptr_t, size_t);

int solo5_app_main(const struct solo5_start_info *si)
{
    solo5_cmdline = si->cmdline;
    _nolibc_init(si->heap_start, si->heap_size);
    caml_startup(unused_argv);

    return 0;
}
