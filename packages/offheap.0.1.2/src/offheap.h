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
typedef void (*free_t) (void *data, void *ptr, size_t size);

/**
 * Default allocator: invokes malloc.
 */
void *offheap_alloc(value allocator, size_t size);

/**
 * Default deallocator: invokes free.
 */
void offheap_free(value allocator, void *ptr, size_t size);

/**
 * Buffer allocated by copying.
 */
typedef struct {
  /// Pointer to the region.
  void *ptr;
  /// Size of the region.
  size_t size;
} offheap_buffer_t;

/**
 * Copies an object off the heap, into a buffer allocated by an allocator.
 */
offheap_buffer_t offheap_copy(
    value v,
    void *data,
    alloc_t allocFn,
    int copyStatic
);
