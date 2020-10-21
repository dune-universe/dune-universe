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

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "bindings.h"
#include "hypercall.h"

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>

#undef GNTTAB_DEBUG
#ifdef GNTTAB_DEBUG
#include <stdio.h>
#define DPRINTF(level, format, ...) \
    if (GNTTAB_DEBUG >= level) printf("%s(): " format, __func__, __VA_ARGS__)
#else
#define DPRINTF(level, format, ...)
#endif

/*
 * Each grant table page has space for 512 entries. 4 pages worth is what the
 * original MiniOS-based gnttab implementation used, so we stick with that for
 * now.
 */
#define NR_GRANT_TABLE_PAGES 4
#define NR_GRANT_TABLE_ENTRIES \
    (NR_GRANT_TABLE_PAGES * PAGE_SIZE / sizeof (grant_entry_v1_t))

static grant_entry_v1_t *gnttab_table;

/*
 * Grant pages imported from other Xen domains must use a separate virtual
 * memory address space. Solo5 maps us an extra 1GB address space at physical
 * address 4GB (see bindings/xen/platform.c), of which we use a suitably sized
 * area for this purpose.
 */
#define NR_GNTMAP_AREA_PAGES (2 * NR_GRANT_TABLE_ENTRIES)
static bmap_allocator_t *gnttab_alloc;

void gnttab_init(void)
{
    int rc = posix_memalign((void **)&gnttab_table, PAGE_SIZE,
            NR_GRANT_TABLE_PAGES * PAGE_SIZE);
    assert (rc == 0);

    for (int i = 0; i < NR_GRANT_TABLE_PAGES; i++) {
        uint64_t gpfn = ((uintptr_t)gnttab_table + i * PAGE_SIZE) >> PAGE_SHIFT;
        rc = hypercall_physmap_add_grant_table(i, gpfn);
        assert (rc == 0);
    }
    DPRINTF(1, "pages = %zd, entries = %zd\n", NR_GRANT_TABLE_PAGES,
            NR_GRANT_TABLE_ENTRIES);

    uint64_t gntmap_area_addr;
    size_t gntmap_area_size;
    solo5__xen_get_gntmap_area(&gntmap_area_addr, &gntmap_area_size);
    assert(NR_GNTMAP_AREA_PAGES <= (gntmap_area_size / PAGE_SIZE));
    gnttab_alloc = bmap_init(gntmap_area_addr, NR_GNTMAP_AREA_PAGES);
    assert(gnttab_alloc);
    DPRINTF(1, "gntmap area: %zd pages @ 0x%lx\n", NR_GNTMAP_AREA_PAGES,
            gntmap_area_addr);
}

/*
 * Mapping (importing) pages from other domains (OS.Xen.Import).
 */

typedef struct {
    domid_t dom;
    grant_handle_t handle;
} gnttab_mapping_t;

_Static_assert((sizeof (grant_handle_t) == sizeof (grant_ref_t)),
        "sizeof grant_handle_t must be same as grant_ref_t");

/*
 * This struct represents a "grant table mapping" of (count) pages mapped
 * contiguously starting at (start_addr). entries[].handle is dual-use:
 *
 * 1. When creating a new mapping with gnttab_map(), the caller passes in the
 * grant_ref_t for each page as entries[].handle.
 * 2. On successful return from gnttab_map(), entries[].handle will contain
 * the grant_handle_t for each page returned by Xen.
 * 3. Thus, the returned mapping can then be passed to gnttab_unmap() when it
 * needs to be freed, and it will pass the individual grant_handle_t for each
 * page to Xen.
 */

typedef struct {
    size_t count;
    void *start_addr;
    bool readonly;
    gnttab_mapping_t entries[];
} gnttab_mapping_head_t;

/*
 * Grant table mappings are represented on the OCaml side by OS.Xen.grant_handle
 * which is an opaque type encapsulating a gnttab_mapping_head_t in an OCaml
 * custom block.
 */
static struct custom_operations gnttab_mapping_ops = {
    "io.mirage.xen.gnttab.mapping",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define Mapping_val(v) ((gnttab_mapping_head_t *) Data_custom_val(v))

static value alloc_mapping(size_t count)
{
    size_t size = sizeof (gnttab_mapping_head_t)
        + count * sizeof (gnttab_mapping_t);

    value result = caml_alloc_custom(&gnttab_mapping_ops, size, 0, 1);
    gnttab_mapping_head_t *mapping = Mapping_val(result);
    mapping->count = 0;
    mapping->start_addr = NULL;
    memset(mapping->entries, 0, count * sizeof (gnttab_mapping_t));
    return result;
}

static int gnttab_unmap(gnttab_mapping_head_t *mapping)
{
    int rc;
    uint64_t addr = (uint64_t)mapping->start_addr;

    assert(mapping->start_addr != NULL);
    assert(mapping->count >= 1);
    for (size_t i = 0; i < mapping->count; i++, addr += PAGE_SIZE) {
        rc = hypercall_gnttab_unmap_grant_ref(addr, 0,
                mapping->entries[i].handle);

        DPRINTF(1, "([%zu] handle = 0x%x addr = 0x%p)\n", i,
                mapping->entries[i].handle, addr);
        if (rc != 0)
            break;
    }
    return rc;
}

static int gnttab_map(gnttab_mapping_head_t *mapping)
{
    int rc;
    uint64_t dev_bus_addr;
    uint64_t addr = (uint64_t)mapping->start_addr;
    size_t unmap_count = 0;

    assert(mapping->start_addr != NULL);
    assert(mapping->count >= 1);
    for (size_t i = 0; i < mapping->count; i++, addr += PAGE_SIZE) {
        grant_ref_t ref = mapping->entries[i].handle;
        rc = hypercall_gnttab_map_grant_ref(addr,
                GNTMAP_host_map | (mapping->readonly ? GNTMAP_readonly : 0),
                ref, mapping->entries[i].dom, &mapping->entries[i].handle,
                &dev_bus_addr);

        DPRINTF(1, "([%zu] ref = 0x%x dom = 0x%x readonly = %d handle = 0x%x addr = 0x%p)\n",
                i, ref, mapping->entries[i].dom, mapping->readonly,
                mapping->entries[i].handle, addr);
        if (rc != 0) {
            if (i >= 1) {
                /*
                 * >= 1 mapping succeeded, we need to unmap some before return.
                 */
                unmap_count = i;
                goto out_unmap;
            }
            else {
                /*
                 * No mappings succeeded, so just return the error.
                 */
                break;
            }
        }
    }
    return rc;

    /*
     * Unmap those mappings that succeeded, taking care to restore the original
     * value of mapping->count after the unmap operation.
     */
out_unmap:
    assert(unmap_count >= 1);
    size_t old_count = mapping->count;
    mapping->count = unmap_count;
    (void)gnttab_unmap(mapping);
    mapping->count = old_count;
    return rc;
}

/*
 * Bigarrays used to represent "I/O pages", i.e. in this case memory belonging
 * to a grant mapping must be declared with the following layout and kind,
 * notably CAML_BA_EXTERNAL to ensure that the GC will not attempt to call
 * free() on finalise.
 */
#define CAML_BA_IO_PAGE (CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL)

/*
 * external map_exn:
 *     unit -> Gntref.t -> domid -> bool -> (grant_handle * Io_page.t)
 *
 * Caller: OS.Xen.Import.map
 */
CAMLprim value
mirage_xen_gnttab_map(value v_unused_ctx, value v_ref, value v_domid,
        value v_writable)
{
    CAMLparam4(v_unused_ctx, v_ref, v_domid, v_writable);
    CAMLlocal3(v_mapping, v_page, v_result);
    grant_ref_t ref = Int_val(v_ref);
    domid_t dom = Int_val(v_domid);
    bool readonly = !Bool_val(v_writable);
    void *addr;
    int rc;

    addr = bmap_alloc(gnttab_alloc, 1);
    if (addr == NULL)
        caml_raise_out_of_memory();

    v_mapping = alloc_mapping(1);
    gnttab_mapping_head_t *mapping = Mapping_val(v_mapping);
    mapping->count = 1;
    mapping->start_addr = addr;
    mapping->readonly = readonly;
    mapping->entries[0].dom = dom;
    mapping->entries[0].handle = ref;
    rc = gnttab_map(mapping);
    if (rc != 0) {
        bmap_free(gnttab_alloc, mapping->start_addr, mapping->count);
        mapping->start_addr = NULL;
        mapping->count = 0;
        caml_failwith("mirage_xen_gnttab_map: failed");
    }

    v_page = caml_ba_alloc_dims(CAML_BA_IO_PAGE, 1, addr, 1 * PAGE_SIZE);

    v_result = caml_alloc_tuple(2);
    Store_field(v_result, 0, v_mapping);
    Store_field(v_result, 1, v_page);
    CAMLreturn(v_result);
}

/*
 * external mapv_exn:
 *     unit -> int array -> bool -> (grant_handle * Io_page.t)
 *
 * The array is an array of (domid_t, grant_ref_t).
 *
 * Caller: OS.Xen.Import.mapv
 */
CAMLprim value
mirage_xen_gnttab_mapv(value v_unused_ctx, value v_array, value v_writable)
{
    CAMLparam3(v_unused_ctx, v_array, v_writable);
    CAMLlocal3(v_mapping, v_page, v_result);
    size_t count = Wosize_val(v_array) / 2;
    bool readonly = !Bool_val(v_writable);
    void *addr;
    int rc;

    addr = bmap_alloc(gnttab_alloc, count);
    if (addr == NULL)
        caml_raise_out_of_memory();

    v_mapping = alloc_mapping(count);
    gnttab_mapping_head_t *mapping = Mapping_val(v_mapping);
    mapping->count = count;
    mapping->start_addr = addr;
    mapping->readonly = readonly;
    for (size_t i = 0; i < count; i++) {
        domid_t dom = Int_val(Field(v_array, i * 2 + 0));
        grant_ref_t ref = Int_val(Field(v_array, i * 2 + 1));
        mapping->entries[i].dom = dom;
        mapping->entries[i].handle = ref;
    }
    rc = gnttab_map(mapping);
    if (rc != 0) {
        bmap_free(gnttab_alloc, mapping->start_addr, count);
        mapping->start_addr = NULL;
        mapping->count = 0;
        caml_failwith("mirage_xen_gnttab_mapv: failed");
    }

    v_page = caml_ba_alloc_dims(CAML_BA_IO_PAGE, 1, addr, count * PAGE_SIZE);

    v_result = caml_alloc_tuple(2);
    Store_field(v_result, 0, v_mapping);
    Store_field(v_result, 1, v_page);
    CAMLreturn(v_result);
}

/*
 * external unmap_exn:
 *     unit -> grant_handle -> unit
 *
 * Caller: OS.Xen.Import.unmap
 */
CAMLprim value
mirage_xen_gnttab_unmap(value v_unused_ctx, value v_mapping)
{
    CAMLparam2(v_unused_ctx, v_mapping);
    gnttab_mapping_head_t *mapping = Mapping_val(v_mapping);
    int rc;

    rc = gnttab_unmap(mapping);
    if (rc != 0)
        caml_failwith("mirage_xen_gnttab_unmap: failed");
    bmap_free(gnttab_alloc, mapping->start_addr, mapping->count);
    mapping->start_addr = 0;
    mapping->count = 0;
    CAMLreturn(Val_unit);
}

CAMLprim value
mirage_xen_gnttab_get_nr_reserved(value unit)
{
    return Val_int(GNTTAB_NR_RESERVED_ENTRIES);
}

CAMLprim value
mirage_xen_gnttab_get_nr_entries(value unit)
{
    return Val_int(NR_GRANT_TABLE_ENTRIES);
}

/*
 * Sharing (exporting) pages with other domains (OS.Xen.Export).
 */

static void gnttab_grant_access(grant_ref_t ref, domid_t domid, uint64_t addr,
        bool readonly)
{
    assert(ref >= GNTTAB_NR_RESERVED_ENTRIES && ref < NR_GRANT_TABLE_ENTRIES);
    assert((addr & (PAGE_SIZE - 1)) == 0);
    grant_entry_v1_t *entry = &gnttab_table[ref];

    entry->frame = addr >> PAGE_SHIFT;
    entry->domid = domid;
    cpu_write_barrier();
    entry->flags = GTF_permit_access | (readonly ? GTF_readonly : 0);

    DPRINTF(2, "(ref = 0x%x, domid = 0x%x, addr = 0x%lx, readonly = %d)\n",
            ref, domid, addr, readonly);
}

/*
 * external grant_access:
 *     Gntref.t -> Io_page.t -> int -> bool -> unit
 *
 * Caller: OS.Xen.Export.grant_access
 */
CAMLprim value
mirage_xen_gnttab_grant_access(value v_ref, value v_iopage, value v_domid,
        value v_writable)
{
    CAMLparam4(v_ref, v_iopage, v_domid, v_writable);
    grant_ref_t ref = Int_val(v_ref);
    uint64_t addr = (uint64_t)Caml_ba_data_val(v_iopage);
    domid_t domid = Int_val(v_domid);
    bool readonly = !Bool_val(v_writable);

    gnttab_grant_access(ref, domid, addr, readonly);
    CAMLreturn(Val_unit);
}

/*
 * Returns false if revoking access failed due to the pages still being in use.
 */
static bool gnttab_end_access(grant_ref_t ref)
{
    assert(ref >= GNTTAB_NR_RESERVED_ENTRIES && ref < NR_GRANT_TABLE_ENTRIES);
    uint16_t *pflags = &gnttab_table[ref].flags;
    uint16_t flags, nflags;
    bool rc = true;

    nflags = *pflags;
    do {
        flags = nflags;
        if (flags & (GTF_reading | GTF_writing)) {
            rc = false;
            break;
        }
    } while (atomic_sync_cmpxchgw(pflags, flags, 0) != flags);

    DPRINTF(2, "(ref = 0x%x) = %d\n", ref, rc);
    return rc;
}

/*
 * external try_end_access:
 *     Gntref.t -> bool
 *
 * Caller: OS.Xen.Export.try_end_access
 */
CAMLprim value
mirage_xen_gnttab_end_access(value v_ref)
{
    CAMLparam1(v_ref);
    grant_ref_t ref = Int_val(v_ref);
    bool rc = gnttab_end_access(ref);

    CAMLreturn(Val_bool(rc));
}
