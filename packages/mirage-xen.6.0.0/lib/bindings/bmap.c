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

#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifndef TEST_STANDALONE
#include "bindings.h"
#else
#define PAGE_SIZE 4096
#endif

/*
 * This is a simple bitmap allocator for virtual memory addresses. Worst-case
 * performance for bmap_alloc() is O(n), where n = total_pages / 8. bmap_free()
 * is O(1) but requires the caller to keep the size of the allocated block.
 */
struct bmap_allocator {
    long *bmap;                          /* 1 bit per page; 1=free, 0=used */
    size_t bmap_size;                    /* # of words in bmap[] */
    uint64_t start_addr;                 /* starting virtual memory address */
};

#define BPW (sizeof(long) * 8)
_Static_assert(sizeof(long) == 8, "long must be 64 bits");

/*
 * Returns 0-based index of first set bit in (bmap[]), starting with the bit
 * index (at), or -1 if none found and end of (bmap[]) was reached.
 */
static int ffs_at(long *bmap, size_t bmap_size, int at)
{
    int word = at / BPW;
    int shift = at % BPW;
    int bit = 0;
    int i;

    for (i = word; i < bmap_size; i++) {
        if (i == word)
            /* (at) is not on a word boundary; shift so we can use ffsl */
            bit = __builtin_ffsl(bmap[i] >> shift);
        else
            bit = __builtin_ffsl(bmap[i]);
        if (bit)
            break;
    }

    if (bit) {
        if (i == word)
            /* Restore previous shift if any */
            bit += shift;
        return (i * BPW) + (bit - 1);
    }
    else
        return -1;
}

/*
 * Returns 0-based index of first clear bit in (bmap[]), starting with the bit
 * index (at), or -1 if none found and end of (bmap[]) was reached.
 */
static int ffc_at(long *bmap, size_t bmap_size, int at)
{
    int word = at / BPW;
    int shift = at % BPW;
    int bit = 0;
    int i;

    for (i = word; i < bmap_size; i++) {
        if (i == word)
            /* (at) is not on a word boundary; shift so we can use ffsl */
            bit = __builtin_ffsl(~bmap[i] >> shift);
        else
            bit = __builtin_ffsl(~bmap[i]);
        if (bit)
            break;
    }

    if (bit) {
        if (i == word)
            /* Restore previous shift if any */
            bit += shift;
        return (i * BPW) + (bit - 1);
    }
    else
        return -1;
}

/*
 * Set (n) bits in (bmap[]) at 0-based bit index (at).
 */
static void setn_at(long *bmap, size_t bmap_size, int at, int n)
{
    assert((at + n - 1) < (bmap_size * BPW));
    while (n > 0) {
        n -= 1;
        bmap[((at + n) / BPW)] |= (1UL << ((at + n) % BPW));
    }
}

/*
 * Clear (n) bits in (bmap[]) at 0-based bit index (at).
 */
static void clearn_at(long *bmap, size_t bmap_size, int at, int n)
{
    assert((at + n - 1) < (bmap_size * BPW));
    while (n > 0) {
        n -= 1;
        bmap[((at + n) / BPW)] &= ~(1UL << ((at + n) % BPW));
    }
}

/*
 * Allocate (n) pages from (alloc), returns a memory address or NULL if no
 * space found.
 */
void *bmap_alloc(bmap_allocator_t *alloc, size_t n)
{
    int a = 0, b = 0;
    size_t bmap_bits = alloc->bmap_size * BPW;

    /*
     * Allocating 0 pages is not allowed.
     */
    assert(n >= 1);

    while (1) {
        /*
         * Look for the first free page starting at (b), initially 0.
         */
        a = ffs_at(alloc->bmap, alloc->bmap_size, b);
        if (a < 0)
            return NULL;
        /*
         * Look for the first used page after the found free page.
         */
        b = ffc_at(alloc->bmap, alloc->bmap_size, a);
        if (b < 0)
            /*
             * Nothing found; all remaining pages from a..bmap_bits are free.
             */
            b = bmap_bits;
        /*
         * Is the block big enough? If yes, mark as used (0) and return it.
         */
        if (b - a >= n) {
            clearn_at(alloc->bmap, alloc->bmap_size, a, n);
            return (void *)(alloc->start_addr + (a * PAGE_SIZE));
        }
        /*
         * Stop the search if we hit the end of bmap[] and did not find a large
         * enough block.
         */
        if (b == bmap_bits)
            return NULL;
        /*
         * If we got here, loop with (b) set to the last seen used page.
         */
    }
}

/*
 * Free (n) pages at (addr) from (alloc).
 */
void bmap_free(bmap_allocator_t *alloc, void *addr, size_t n)
{
    /*
     * Verify that:
     *    addr is page-aligned
     *    addr is within the range given to alloc
     *    n is at least 1; the maximum size of n is checked in setn_at().
     */
    assert(((uintptr_t)addr & (PAGE_SIZE - 1)) == 0);
    assert((uintptr_t)addr >= alloc->start_addr);
    assert(n >= 1);

    int a = ((uintptr_t)addr - alloc->start_addr) / PAGE_SIZE;
    setn_at(alloc->bmap, alloc->bmap_size, a, n);
}

/*
 * Initialise the allocator to use (n_pages) at (start_addr).
 */
bmap_allocator_t *bmap_init(uint64_t start_addr, size_t n_pages)
{
    bmap_allocator_t *alloc = malloc(sizeof (bmap_allocator_t));
    assert(alloc != NULL);
    /*
     * n_pages must be a multiple of BPW.
     */
    assert((n_pages % BPW) == 0);
    alloc->bmap_size = n_pages / BPW;
    alloc->bmap = malloc(alloc->bmap_size * sizeof(long));
    assert(alloc->bmap);
    alloc->start_addr = start_addr;
    /*
     * All pages are initially free; set all bits in bmap[].
     */
    memset(alloc->bmap, 0xff, alloc->bmap_size * sizeof(long));

    return alloc;
}

#ifdef TEST_STANDALONE
/*
 * For standalone testing of the algorithm.
 */

#include <stdio.h>

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("need allocation size\n");
        return 1;
    }

    int n = atoi(argv[1]);
    bmap_allocator_t *alloc = bmap_init(0x100000, 4096);

    char *a;
    int c = 0;
    do {
        a = bmap_alloc(alloc, n);
        if (a)
            printf ("%d: %p - %p\n", ++c, a, a + (n * PAGE_SIZE) - 1);
    } while (a);

    /* Keep valgrind happy when testing standalone. */
    free(alloc->bmap);
    free(alloc);
}

#endif
