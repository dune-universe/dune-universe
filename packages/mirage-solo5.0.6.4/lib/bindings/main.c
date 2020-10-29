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
static size_t solo5_heap_size;
static uintptr_t sp_at_start;

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

/*
 * Caller: OS.Memory, @@noalloc
 */
CAMLprim value
mirage_memory_get_heap_words(value v_unit)
{
    return Val_long(solo5_heap_size / sizeof(value));
}

extern size_t malloc_footprint(void);

/*
 * Caller: OS.Memory, @@noalloc
 */
CAMLprim value
mirage_memory_get_live_words(value v_unit)
{
    return Val_long(malloc_footprint() / sizeof(value));
}

/*
 * Caller: OS.Memory, @@noalloc
 *
 * The implementation currently uses a hard-coded value for the stack guard
 * size; this must be kept in sync with nolibc's sbrk() implementation.
 * TODO: Consider providing a formal interface for this.
 */
CAMLprim value
mirage_memory_get_stack_words(value v_unit)
{
    int dummy;

    return Val_long((sp_at_start - (uintptr_t)&dummy + 0x100000)
            / sizeof(value));
}

extern void _nolibc_init(uintptr_t, size_t);

int solo5_app_main(const struct solo5_start_info *si)
{
    int dummy;

    sp_at_start = (uintptr_t)&dummy;
    _nolibc_init(si->heap_start, si->heap_size);
    solo5_heap_size = si->heap_size;
    solo5_cmdline = si->cmdline;
    caml_startup(unused_argv);

    return 0;
}
