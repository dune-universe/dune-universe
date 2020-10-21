/*
 * Copyright (c) 2020 Martin Lucina <martin@lucina.net>
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
#include "bindings.h"
#include "hypercall.h"
#include "xen/hvm/params.h"

#include <stdlib.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

static char *unused_argv[] = { "mirage", NULL };
static const char *solo5_cmdline = "";
static size_t solo5_heap_size;
static uintptr_t sp_at_start;

CAMLprim value
mirage_xen_get_cmdline(value v_unit)
{
    CAMLparam1(v_unit);

    CAMLreturn(caml_copy_string(solo5_cmdline));
}

/*
 * Bigarrays used to represent "I/O pages", i.e. in this case memory belonging
 * to the Xenstore and initial console rings must be declared with the following
 * layout and kind, notably CAML_BA_EXTERNAL to ensure that the GC will not
 * attempt to call free() on finalise.
 */
#define CAML_BA_IO_PAGE (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL)

CAMLprim value
mirage_xen_get_console_evtchn(value v_unit)
{
    CAMLparam1(v_unit);
    uint64_t raw_evtchn = 0;
    int rc;

    rc = hypercall_hvm_get_param(HVM_PARAM_CONSOLE_EVTCHN, &raw_evtchn);
    if (rc)
        caml_failwith("hvm_get_param HVM_PARAM_CONSOLE_EVTCHN");
    else
        CAMLreturn(Val_int(raw_evtchn));
}

CAMLprim value
mirage_xen_get_console_page(value v_unit)
{
    CAMLparam1(v_unit);
    uint64_t raw_pfn = 0;
    int rc;

    rc = hypercall_hvm_get_param(HVM_PARAM_CONSOLE_PFN, &raw_pfn);
    if (rc)
        caml_failwith("hvm_get_param HVM_PARAM_CONSOLE_PFN");
    else
        CAMLreturn(caml_ba_alloc_dims(CAML_BA_IO_PAGE, 1,
                    (void *)(raw_pfn << PAGE_SHIFT), PAGE_SIZE));
}

CAMLprim value
mirage_xen_get_xenstore_evtchn(value v_unit)
{
    CAMLparam1(v_unit);
    uint64_t raw_evtchn = 0;
    int rc;

    rc = hypercall_hvm_get_param(HVM_PARAM_STORE_EVTCHN, &raw_evtchn);
    if (rc)
        caml_failwith("hvm_get_param HVM_PARAM_STORE_EVTCHN");
    else
        CAMLreturn(Val_int(raw_evtchn));
}

CAMLprim value
mirage_xen_get_xenstore_page(value v_unit)
{
    CAMLparam1(v_unit);
    uint64_t raw_pfn = 0;
    int rc;

    rc = hypercall_hvm_get_param(HVM_PARAM_STORE_PFN, &raw_pfn);
    if (rc)
        caml_failwith("hvm_get_param HVM_PARAM_STORE_PFN");
    else
        CAMLreturn(caml_ba_alloc_dims(CAML_BA_IO_PAGE, 1,
                    (void *)(raw_pfn << PAGE_SHIFT), PAGE_SIZE));
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
    gnttab_init();
    solo5_cmdline = si->cmdline;
    caml_startup(unused_argv);

    return 0;
}
