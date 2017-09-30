/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef OCAML_DTOA_BIGNUM_H_
#define OCAML_DTOA_BIGNUM_H_

#include <stdbool.h>
#include <stdint.h>

#include "utils.h"

typedef uint32_t Chunk;
typedef uint64_t DoubleChunk;

extern const int kBigNumMaxSignificantBits;
extern const int kBigitSize;
extern const Chunk kBigitMask;
extern const int kBigitCapacity;

typedef struct {
  Chunk bigits[128]; // 128 = kBigitCapacity
  int used_digits;
  // The Bignum's value equals value(bigits_) * 2^(exponent_ * kBigitSize).
  int exponent;
} bignum;

bignum new_bignum();

void bignum_assign_uint16(bignum* num, uint16_t value);
void bignum_assign_uint64(bignum* num, uint64_t value);
void bignum_assign_bignum(bignum* num, bignum other);

void bignum_assign_decimal_string(bignum* num, const char* value);
void bignum_assign_hex_string(bignum* num, const char* value);

void bignum_assign_power_uint16(bignum* num, uint16_t base, int power_exponent);

void bignum_add_uint64(bignum* num, uint64_t operand);
void bignum_add_bignum(bignum* num, bignum other);
// Precondition: this >= other.
void bignum_subtract_bignum(bignum* num, bignum other);

void bignum_square(bignum* num);
void bignum_shift_left(bignum* num, int shift_amount);
void bignum_multiply_by_uint32(bignum* num, uint32_t factor);
void bignum_multiply_by_uint64(bignum* num, uint64_t factor);
void bignum_multiply_by_power_of_ten(bignum* num, int exponent);
void bignum_times_10(bignum* num);
// Pseudocode:
//  int result = this / other;
//  this = this % other;
// In the worst case this function is in O(this/other).
uint16_t bignum_divide_modulo_int_bignum(bignum* num, bignum other);

bool bignum_to_hex_string(bignum num, char* buffer, int buffer_size);

// Returns
//  -1 if a < b,
//   0 if a == b, and
//  +1 if a > b.
int bignum_compare(bignum a, bignum b);
bool bignum_equal(bignum a, bignum b);
bool bignum_less_equal(bignum a, bignum b);
bool bignum_less(bignum a, bignum b);
// Returns Compare(a + b, c);
int bignum_plus_compare(bignum a, bignum b, bignum c);
// Returns a + b == c
bool bignum_plus_equal(bignum a, bignum b, bignum c);
// Returns a + b <= c
bool bignum_plus_less_equal(bignum a, bignum b, bignum c);
// Returns a + b < c
bool bignum_plus_less(bignum a, bignum b, bignum c);

#endif  // OCAML_DTOA_BIGNUM_H_
