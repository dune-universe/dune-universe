#include <stdint.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

static inline uint32_t r(uint32_t a, int b) {
  int rs = 32 - b;
  return (a << b) | (a >> rs);
}

static inline void quarterround(uint32_t *x, int a, int b, int c, int d) {
  x[a] += x[b]; x[d] = r(x[d] ^ x[a], 16);
  x[c] += x[d]; x[b] = r(x[b] ^ x[c], 12);
  x[a] += x[b]; x[d] = r(x[d] ^ x[a], 8);
  x[c] += x[d]; x[b] = r(x[b] ^ x[c], 7);
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

static void chacha_core(int count, uint8_t *src, uint8_t *dst) {
  uint32_t x[16];
  for (int i = 0; i < 16; i++) {
    x[i] = get_u32_le(src, i * 4);
  }
  for (int i = 0; i < count; i++) {
    quarterround(x, 0, 4, 8, 12);
    quarterround(x, 1, 5, 9, 13);
    quarterround(x, 2, 6, 10, 14);
    quarterround(x, 3, 7, 11, 15);

    quarterround(x, 0, 5, 10, 15);
    quarterround(x, 1, 6, 11, 12);
    quarterround(x, 2, 7, 8, 13);
    quarterround(x, 3, 4, 9, 14);
  }
  for (int i = 0; i < 16; i++) {
    uint32_t xi = x[i];
    uint32_t hj = get_u32_le(src, i * 4);
    set_u32_le(dst, i * 4, xi + hj);
  }
}

CAMLprim value
caml_chacha_core(value count, value src, value dst)
{
  chacha_core(Int_val(count), Caml_ba_data_val(src), Caml_ba_data_val(dst));
  return Val_unit;
}

