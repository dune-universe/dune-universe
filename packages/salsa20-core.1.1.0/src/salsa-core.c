#include <stdint.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

static inline uint32_t r(uint32_t a, int b) {
  int rs = 32 - b;
  return (a << b) | (a >> rs);
}

static inline uint32_t combine(uint32_t y0, uint32_t y1, uint32_t y2, int shift) {
  return r(y1 + y2, shift) ^ y0;
}

static inline void quarterround(uint32_t *x, int y0, int y1, int y2, int y3) {
  x[y1] = combine(x[y1], x[y0], x[y3], 7);
  x[y2] = combine(x[y2], x[y1], x[y0], 9);
  x[y3] = combine(x[y3], x[y2], x[y1], 13);
  x[y0] = combine(x[y0], x[y3], x[y2], 18);
}

static inline uint32_t get_u32_le(uint8_t *input, int offset) {
  return input[offset]
    | (input[offset + 1] << 8)
    | (input[offset + 2] << 16)
    | (input[offset + 3] << 24);
}

static inline void set_u32_le(uint8_t *input, int offset, uint32_t value) {
  input[offset] = (uint8_t) value;
  input[offset + 1] = (uint8_t) (value >> 8);
  input[offset + 2] = (uint8_t) (value >> 16);
  input[offset + 3] = (uint8_t) (value >> 24);
}

static void salsa_core(int count, uint8_t *src, uint8_t *dst) {
  uint32_t x[16];
  for (int i = 0; i < 16; i++) {
    x[i] = get_u32_le(src, i * 4);
  }
  for (int i = 0; i < count; i++) {
    quarterround(x, 0, 4, 8, 12);
    quarterround(x, 5, 9, 13, 1);
    quarterround(x, 10, 14, 2, 6);
    quarterround(x, 15, 3, 7, 11);

    quarterround(x, 0, 1, 2, 3);
    quarterround(x, 5, 6, 7, 4);
    quarterround(x, 10, 11, 8, 9);
    quarterround(x, 15, 12, 13, 14);
  }
  for (int i = 0; i < 16; i++) {
    uint32_t xi = x[i];
    uint32_t hj = get_u32_le(src, i * 4);
    set_u32_le(dst, i * 4, xi + hj);
  }
}

CAMLprim value
caml_salsa_core(value count, value src, value dst)
{
  salsa_core(Int_val(count), Caml_ba_data_val(src), Caml_ba_data_val(dst));
  return Val_unit;
}
