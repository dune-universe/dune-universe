// Copies objects out of the OCaml heap where they are not managed by the GC

#include <string.h>
#include <assert.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/address_class.h>
#include <caml/gc.h>

#include "offheap.h"



#define ENTRIES_PER_QUEUE_CHUNK 4096



/**
 * Item stored in the queue.
 */
typedef struct queue_item {
  /// Pointer to the copied block.
  value value;
  /// Saved header value.
  header_t header;
} queue_item_t;


/**
 * Linked list of blocks of pointers.
 */
typedef struct queue_chunk {
  /// Link to the next chunk storing the queue.
  struct queue_chunk *next;
  /// Storage for a number of pointer.
  struct queue_item entries[ENTRIES_PER_QUEUE_CHUNK];
} queue_chunk_t;


/**
 * Wrapper on top of the queue implementing iteration.
 */
typedef struct queue {
  /// Pointer to the head of the queue.
  queue_chunk_t first_chunk;
  /// Pointer to the chunk being read.
  queue_chunk_t *read_chunk;
  /// Pointer to the chunk written.
  queue_chunk_t *write_chunk;
  /// Index into the read chunk.
  int read_pos;
  /// Index into the written chunk.
  int write_pos;
} queue_t;


/**
 * Default allocator: invokes malloc.
 */
static void *default_alloc(value allocator, size_t size)
{
  return malloc(size);
}

/**
 * Default deallocator: invokes free.
 */
static void default_free(value allocator, void *ptr)
{
  free(ptr);
}

/**
 * Creates a new queue.
 */
void queue_init(queue_t *q)
{
  q->first_chunk.next = NULL;
  q->read_chunk = &q->first_chunk;
  q->write_chunk = &q->first_chunk;
  q->read_pos = 0;
  q->write_pos = 0;
}

/**
 * Adds an item to the end of the queue.
 */
int queue_push(queue_t *q, value v, header_t hdr)
{
  if (q->write_pos == ENTRIES_PER_QUEUE_CHUNK) {
    struct queue_chunk *new_chunk = malloc(sizeof(struct queue_chunk));
    if (new_chunk == NULL) {
      return 0;
    }
    new_chunk->next = NULL;
    q->write_chunk->next = new_chunk;
    q->write_pos = 0;
    q->write_chunk = new_chunk;
  }

  struct queue_item *item = &q->write_chunk->entries[q->write_pos++];
  item->value = v;
  item->header = hdr;
  return 1;
}

/**
 * Returns the next item from the front of the queue.
 */
queue_item_t queue_pop(queue_t *q)
{
  if (q->read_pos == ENTRIES_PER_QUEUE_CHUNK) {
    q->read_pos = 0;
    q->read_chunk = q->read_chunk->next;
  }
  return q->read_chunk->entries[q->read_pos++];
}

/**
 * Checks if there are any more items to read.
 */
int queue_empty(queue_t *q)
{
  return q->read_pos == q->write_pos && q->read_chunk == q->write_chunk;
}

/**
 * Resets the front pointer to the start of the queue.
 */
void queue_reset(queue_t *q)
{
  q->read_pos = 0;
  q->read_chunk = &q->first_chunk;
}

/**
 * Frees storage used by the queue.
 */
void queue_free(queue_t *q)
{
  struct queue_chunk *chunk = q->first_chunk.next;
  while (chunk) {
    struct queue_chunk *next = chunk->next;
    free(chunk);
    chunk = next;
  }
}

/**
 * Checks if the object can be copied.
 */
static inline int can_copy(tag_t tag)
{
  return tag != Custom_tag && tag != Abstract_tag;
}


/**
 * Copies an object into a buffer, returning a pointer to the buffer.
 */
void *offheap_copy(value v, void *data, alloc_t alloc_fn)
{
  static struct queue q;
  queue_init(&q);

  // Some objects cannot be copied - bail out.
  if (!can_copy(Tag_val(v))) {
    return NULL;
  }

  // Adjust pointer for infix objects.
  if (Tag_hd(Hd_val(v)) == Infix_tag) {
    v -= Infix_offset_hd(Hd_val(v));
  }

  // Push the first item with offset 0 and marks its header.
  queue_push(&q, v, Hd_val(v));
  Hd_val(v) = Make_header(0, Tag_val(v), Caml_blue);

  // First pass: traverse the object and compute the size of the final copy,
  // in bytes. The queue will contain the unrolled list of all blocks.
  intnat size = 0;
  while (!queue_empty(&q)) {
    // Pop the next element from the queue and read information from the header
    // before the size in the header is overwritten with the new offset of the
    // object into the buffer in which the copy will be allocated.
    const queue_item_t item = queue_pop(&q);
    const value node = item.value;
    const header_t hd = item.header;
    const mlsize_t sz = Wosize_hd(hd);
    const tag_t tag = Tag_hd(hd);

    // Advance the pointer by the size of the block. Since we inspect the colour
    // encoded in the header when checking pointers to this object, we keep the
    // header correctly formatted, with colour and tag intact.
    const mlsize_t bytes = Bhsize_hd(hd);
    Hd_val(node) = Make_header(size, tag, Caml_blue);
    size += bytes;

    if (tag < No_scan_tag) {
      // Push the interesting fields on the queue.
      for (int i = 0; i < sz; ++i) {
        const value field = Field(node, i);

        // Skip over primitive fields and non-heap pointers.
        if (!Is_block(field) || !Is_in_heap_or_young(field)) {
          continue;
        }

        const header_t fh = Hd_val(field);
        const tag_t th = Tag_hd(fh);

        // Off-heap objects cannot point back to the heap and we cannot copy
        // custom objects as we do not know anything about their internals.
        if (!can_copy(th)) {
          size = -2;
          goto error;
        }

        // Skip already visited objects: their header is blue.
        if (Color_hd(fh) == Caml_blue) {
          continue;
        }

        // Add the item to the queue and mark the chunk as visited.
        if (!queue_push(&q, field, fh)) {
          size = -1;
          goto error;
        }
        Hd_val(field) = Bluehd_hd(fh);
      }
    }
  }

  // Now that the size is known, allocate a contiguous off-heap
  // buffer large enough to hold the entire object.
  const uintptr_t buffer = (uintptr_t)alloc_fn(data, size);

  // Second pass: Adjust all pointers.
  if (buffer) {
    uintptr_t ptr = buffer;
    queue_reset(&q);
    while (!queue_empty(&q)) {
      // Pop the pointer and its old header.
      const queue_item_t item = queue_pop(&q);
      const value node = item.value;
      const header_t hd = item.header;
      const mlsize_t sz = Wosize_hd(hd);
      const tag_t tag = Tag_hd(hd);

      // Get a pointer to the current value and advance the offset.
      const value dst = Val_hp((void*)ptr);
      Hd_val(dst) = hd;
      ptr += Bhsize_hd(hd);

      // Set up the new object.
      if (tag < No_scan_tag) {
        for (int i = 0; i < sz; ++i) {
          value field = Field(node, i);

          if (!Is_block(field) || !Is_in_heap_or_young(field)) {
            // Simply copy over primitives and off-heap pointers.
            Field(dst, i) = field;
          } else {
            if (Tag_hd(Hd_val(field)) == Infix_tag){
              field -= Infix_offset_hd(Hd_val(field));
            }

            // At this point, all pointed-to objects should be coloured.
            const header_t fd = Hd_val(field);
            assert(Is_blue_hd(fd));

            // Size field of header stores the offset into the new buffer.
            const uintptr_t addr = buffer + Wosize_hd(fd);
            Field(dst, i) = Val_hp((void*)addr);
          }
        }
      } else {
        // If not a tuple of fields, copy raw data.
        memcpy(Bp_val(dst), Bp_val(node), Bosize_hd(hd));
      }
    }

    assert(ptr == buffer + size);
  }

error:
  // Third pass: reset all the headers.
  queue_reset(&q);
  while (!queue_empty(&q)) {
    queue_item_t item = queue_pop(&q);
    Hd_val(item.value) = item.header;
  }
  queue_free(&q);

  return size < 0 ? NULL : (void *)buffer;
}

CAMLprim value offheap_copy_with_alloc(value allocator, value obj)
{
  CAMLparam2(allocator, obj);
  CAMLlocal1(proxy);

  // If the object is not on the OCaml heap, return it unchanged.
  if (!Is_block(obj) || !Is_in_heap_or_young(obj)) {
    return obj;
  }

  // Fetch the allocator function.
  alloc_t alloc_fn = (alloc_t)Field(allocator, 0);

  // Copy the object.
  void *copy = offheap_copy(obj, (void *)allocator, alloc_fn);
  if (copy == NULL) {
    caml_invalid_argument("object could not be copied off-heap");
  }

  // Create a proxy pointing to the object, storing the allocator as well.
  proxy = caml_alloc_small(2, Abstract_tag);
  Field(proxy, 0) = (value)copy;
  Field(proxy, 1) = allocator;
  CAMLreturn(proxy);
}

CAMLprim value offheap_get(value obj)
{
  CAMLparam1(obj);
  CAMLlocal1(ptr);

  // If the object is not on the OCaml heap, return it unchanged.
  if (!Is_block(obj) || !Is_in_heap_or_young(obj)) {
    return obj;
  }

  // Fetch the pointer, which should not have been deleted.
  ptr = *Op_val(obj);
  if (Is_long(ptr)) {
    caml_invalid_argument("deleted");
  }

  // Return a pointer to the header.
  CAMLreturn(Val_hp(ptr));
}

CAMLprim value offheap_delete(value obj)
{
  CAMLparam1(obj);
  CAMLlocal2(allocator, ptr);

  // If object is not on the OCaml heap, do nothing.
  if (!Is_block(obj) || !Is_in_heap_or_young(obj)) {
    CAMLreturn(Val_unit);
  }

  // Fetch the pointer, which should not have been deleted.
  ptr = *Op_val(obj);
  if (Is_long(ptr)) {
    caml_invalid_argument("deleted");
  }

  // Free the pointer, which should be a valid malloc'd pointer.
  allocator = Field(obj, 1);
  free_t free_fn = (free_t)Field(allocator, 1);
  free_fn((void *)allocator, (void*)ptr);

  // Clear the field.
  Field(obj, 0) = Val_long(0);

  CAMLreturn(Val_unit);
}

CAMLprim value offheap_get_alloc(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(block);

  // Allocate a block with two fields to hold the default allocator.
  // Custom allocators can store additional data by using a larger object.
  block = caml_alloc_small(2, Abstract_tag);
  Field(block, 0) = (value)default_alloc;
  Field(block, 1) = (value)default_free;
  CAMLreturn(block);
}
