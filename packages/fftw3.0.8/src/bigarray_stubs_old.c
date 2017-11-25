/* Functions that where not exported (even with CAML_INTERNALS) prior
 * to OCaml 4.06. */

static uintnat caml_ba_num_elts(struct caml_ba_array * b)
{
  uintnat num_elts;
  int i;
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  return num_elts;
}

CAMLextern int caml_ba_element_size[];

static int caml_ba_compare(value v1, value v2)
{
  struct caml_ba_array * b1 = Caml_ba_array_val(v1);
  struct caml_ba_array * b2 = Caml_ba_array_val(v2);
  uintnat n, num_elts;
  intnat flags1, flags2;
  int i;

  /* Compare kind & layout in case the arguments are of different types */
  flags1 = b1->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK);
  flags2 = b2->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK);
  if (flags1 != flags2) return flags2 - flags1;
  /* Compare number of dimensions */
  if (b1->num_dims != b2->num_dims) return b2->num_dims - b1->num_dims;
  /* Same number of dimensions: compare dimensions lexicographically */
  for (i = 0; i < b1->num_dims; i++) {
    intnat d1 = b1->dim[i];
    intnat d2 = b2->dim[i];
    if (d1 != d2) return d1 < d2 ? -1 : 1;
  }
  /* Same dimensions: compare contents lexicographically */
  num_elts = caml_ba_num_elts(b1);

#define DO_INTEGER_COMPARISON(type) \
  { type * p1 = b1->data; type * p2 = b2->data; \
    for (n = 0; n < num_elts; n++) { \
      type e1 = *p1++; type e2 = *p2++; \
      if (e1 < e2) return -1; \
      if (e1 > e2) return 1; \
    } \
    return 0; \
  }
#define DO_FLOAT_COMPARISON(type) \
  { type * p1 = b1->data; type * p2 = b2->data; \
    for (n = 0; n < num_elts; n++) { \
      type e1 = *p1++; type e2 = *p2++; \
      if (e1 < e2) return -1; \
      if (e1 > e2) return 1; \
      if (e1 != e2) { \
        caml_compare_unordered = 1; \
        if (e1 == e1) return 1; \
        if (e2 == e2) return -1; \
      } \
    } \
    return 0; \
  }

  switch (b1->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_COMPLEX32:
    num_elts *= 2; /*fallthrough*/
  case CAML_BA_FLOAT32:
    DO_FLOAT_COMPARISON(float);
  case CAML_BA_COMPLEX64:
    num_elts *= 2; /*fallthrough*/
  case CAML_BA_FLOAT64:
    DO_FLOAT_COMPARISON(double);
#ifdef CAML_BA_CHAR
  case CAML_BA_CHAR:
    DO_INTEGER_COMPARISON(uint8);
#endif
  case CAML_BA_SINT8:
    DO_INTEGER_COMPARISON(int8);
  case CAML_BA_UINT8:
    DO_INTEGER_COMPARISON(uint8);
  case CAML_BA_SINT16:
    DO_INTEGER_COMPARISON(int16);
  case CAML_BA_UINT16:
    DO_INTEGER_COMPARISON(uint16);
  case CAML_BA_INT32:
    DO_INTEGER_COMPARISON(int32_t);
  case CAML_BA_INT64:
    DO_INTEGER_COMPARISON(int64_t);
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
    DO_INTEGER_COMPARISON(intnat);
  default:
    Assert(0);
    return 0;                   /* should not happen */
  }
#undef DO_INTEGER_COMPARISON
#undef DO_FLOAT_COMPARISON
}

/* Hashing of a bigarray */

#ifdef HASH_EXISTS
#include <caml/hash.h>

static intnat caml_ba_hash(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  intnat num_elts, n;
  uint32_t h, w;
  int i;

  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  h = 0;

  switch (b->flags & CAML_BA_KIND_MASK) {
#ifdef CAML_BA_CHAR
  case CAML_BA_CHAR:
#endif
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: {
    uint8 * p = b->data;
    if (num_elts > 256) num_elts = 256;
    for (n = 0; n + 4 <= num_elts; n += 4, p += 4) {
      w = p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
      h = caml_hash_mix_uint32(h, w);
    }
    w = 0;
    switch (num_elts & 3) {
    case 3: w  = p[2] << 16;    /* fallthrough */
    case 2: w |= p[1] << 8;     /* fallthrough */
    case 1: w |= p[0];
            h = caml_hash_mix_uint32(h, w);
    }
    break;
  }
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: {
    uint16 * p = b->data;
    if (num_elts > 128) num_elts = 128;
    for (n = 0; n + 2 <= num_elts; n += 2, p += 2) {
      w = p[0] | (p[1] << 16);
      h = caml_hash_mix_uint32(h, w);
    }
    if ((num_elts & 1) != 0)
      h = caml_hash_mix_uint32(h, p[0]);
    break;
  }
  case CAML_BA_INT32:
  {
    uint32_t * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_uint32(h, *p);
    break;
  }
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
  {
    intnat * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_intnat(h, *p);
    break;
  }
  case CAML_BA_INT64:
  {
    int64_t * p = b->data;
    if (num_elts > 32) num_elts = 32;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_int64(h, *p);
    break;
  }
  case CAML_BA_COMPLEX32:
    num_elts *= 2;              /* fallthrough */
  case CAML_BA_FLOAT32:
  {
    float * p = b->data;
    if (num_elts > 64) num_elts = 64;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_float(h, *p);
    break;
  }
  case CAML_BA_COMPLEX64:
    num_elts *= 2;              /* fallthrough */
  case CAML_BA_FLOAT64:
  {
    double * p = b->data;
    if (num_elts > 32) num_elts = 32;
    for (n = 0; n < num_elts; n++, p++) h = caml_hash_mix_double(h, *p);
    break;
  }
  }
  return h;
}

#else /* <caml/hash.h> does not exist, use 3.12 hash function */

static intnat caml_ba_hash(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  intnat num_elts, n, h;
  int i;

  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  if (num_elts >= 50) num_elts = 50;
  h = 0;

#define COMBINE(h,v) ((h << 4) + h + (v))

  switch (b->flags & CAML_BA_KIND_MASK) {
  case CAML_BA_SINT8:
  case CAML_BA_UINT8: {
    uint8 * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case CAML_BA_SINT16:
  case CAML_BA_UINT16: {
    uint16 * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case CAML_BA_FLOAT32:
  case CAML_BA_COMPLEX32:
  case CAML_BA_INT32:
#ifndef ARCH_SIXTYFOUR
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
#endif
  {
    uint32_t * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
  case CAML_BA_FLOAT64:
  case CAML_BA_COMPLEX64:
  case CAML_BA_INT64:
#ifdef ARCH_SIXTYFOUR
  case CAML_BA_CAML_INT:
  case CAML_BA_NATIVE_INT:
#endif
#ifdef ARCH_SIXTYFOUR
  {
    uintnat * p = b->data;
    for (n = 0; n < num_elts; n++) h = COMBINE(h, *p++);
    break;
  }
#else
  {
    uint32 * p = b->data;
    for (n = 0; n < num_elts; n++) {
#ifdef ARCH_BIG_ENDIAN
      h = COMBINE(h, p[1]); h = COMBINE(h, p[0]); p += 2;
#else
      h = COMBINE(h, p[0]); h = COMBINE(h, p[1]); p += 2;
#endif
    }
    break;
  }
#endif
  }
#undef COMBINE
  return h;
}

#endif /* HASH_EXISTS */


static void caml_ba_serialize_longarray(void * data,
                                        intnat num_elts,
                                        intnat min_val, intnat max_val)
{
#ifdef ARCH_SIXTYFOUR
  int overflow_32 = 0;
  intnat * p, n;
  for (n = 0, p = data; n < num_elts; n++, p++) {
    if (*p < min_val || *p > max_val) { overflow_32 = 1; break; }
  }
  if (overflow_32) {
    caml_serialize_int_1(1);
    caml_serialize_block_8(data, num_elts);
  } else {
    caml_serialize_int_1(0);
    for (n = 0, p = data; n < num_elts; n++, p++)
      caml_serialize_int_4((int32_t) *p);
  }
#else
  caml_serialize_int_1(0);
  caml_serialize_block_4(data, num_elts);
#endif
}

static void caml_ba_serialize(value v,
                              uintnat * wsize_32,
                              uintnat * wsize_64)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  intnat num_elts;
  int i;

  /* Serialize header information */
  caml_serialize_int_4(b->num_dims);
  caml_serialize_int_4(b->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK));
  /* On a 64-bit machine, if any of the dimensions is >= 2^32,
     the size of the marshaled data will be >= 2^32 and
     extern_value() will fail.  So, it is safe to write the dimensions
     as 32-bit unsigned integers. */
  for (i = 0; i < b->num_dims; i++) caml_serialize_int_4(b->dim[i]);
  /* Compute total number of elements */
  num_elts = 1;
  for (i = 0; i < b->num_dims; i++) num_elts = num_elts * b->dim[i];
  /* Serialize elements */
  switch (b->flags & CAML_BA_KIND_MASK) {
#ifdef CAML_BA_CHAR
  case CAML_BA_CHAR:
#endif
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    caml_serialize_block_1(b->data, num_elts); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    caml_serialize_block_2(b->data, num_elts); break;
  case CAML_BA_FLOAT32:
  case CAML_BA_INT32:
    caml_serialize_block_4(b->data, num_elts); break;
  case CAML_BA_COMPLEX32:
    caml_serialize_block_4(b->data, num_elts * 2); break;
  case CAML_BA_FLOAT64:
  case CAML_BA_INT64:
    caml_serialize_block_8(b->data, num_elts); break;
  case CAML_BA_COMPLEX64:
    caml_serialize_block_8(b->data, num_elts * 2); break;
  case CAML_BA_CAML_INT:
    caml_ba_serialize_longarray(b->data, num_elts, -0x40000000, 0x3FFFFFFF);
    break;
  case CAML_BA_NATIVE_INT:
    caml_ba_serialize_longarray(b->data, num_elts, -0x80000000, 0x7FFFFFFF);
    break;
  }
  /* Compute required size in OCaml heap.  Assumes struct caml_ba_array
     is exactly 4 + num_dims words */
  Assert(SIZEOF_BA_ARRAY == 4 * sizeof(value));
  *wsize_32 = (4 + b->num_dims) * 4;
  *wsize_64 = (4 + b->num_dims) * 8;
}
