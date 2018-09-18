// Copies objects out of the OCaml heap where they are not managed by the GC

#pragma once

#include <caml/config.h>
#include <caml/mlvalues.h>



/**
 * Pointers to an allocator callback.
 */
typedef void *(*alloc_t) (void *data, size_t size);

/**
 * Pointer to a deallocator callback.
 */
typedef void (*free_t) (void *data, void *ptr);

/**
 * Copies an object off the heap, into a buffer allocated by an allocator.
 */
void *offheap_copy(value v, void *data, alloc_t alloc_fn);
