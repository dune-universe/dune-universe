/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>
#include <string.h>

#include "bignum.h"
#include "utils.h"

// 3584 = 128 * 28. We can represent 2^3584 > 10^1000 accurately.
// This bignum can encode much bigger numbers, since it contains an
// exponent.
const int kBigNumMaxSignificantBits = 3584;

// With bigit size of 28 we loose some bits, but a double still fits easily
// into two chunks, and more importantly we can use the Comba multiplication.
const int kBigitSize = 28;
const Chunk kBigitMask = 268435455; // (1 << kBigitSize) - 1;
// Every instance allocates kBigitLength chunks on the stack. Bignums cannot
// grow. There are no checks if the stack-allocated space is sufficient.
const int kBigitCapacity = 128; // kBigNumMaxSignificantBits / kBigitSize;


static const int kBigNumChunkSize = sizeof(Chunk) * 8;
static const int kBigNumDoubleChunkSize = sizeof(DoubleChunk) * 8;

bignum new_bignum() {
  bignum num;
  num.used_digits = 0;
  num.exponent = 0;
  for (int i = 0; i < kBigitCapacity; ++i) {
    num.bigits[i] = 0;
  }
  return num;
}


static int uint16_bit_size(uint16_t value) {
  return 8 * sizeof(value);
}

static int max_int(int a, int b) {
  return a < b ? b : a;
}

static int min_int(int a, int b) {
  return a < b ? a : b;
}

// bigit_length includes the "hidden" digits encoded in the exponent.
int bigit_length(bignum num) { return num.used_digits + num.exponent; }

void bignum_zero(bignum* num) {
  for (int i = 0; i < num->used_digits; ++i) {
    num->bigits[i] = 0;
  }
  num->used_digits = 0;
  num->exponent = 0;
}


void bignum_ensure_capacity(int size) {
  if (size > kBigitCapacity) {
    UNREACHABLE();
  }
}

void bignum_clamp(bignum* num) {
  while (num->used_digits > 0 && num->bigits[num->used_digits - 1] == 0) {
    num->used_digits--;
  }
  if (num->used_digits == 0) {
    // Zero.
    num->exponent = 0;
  }
}


int bignum_is_clamped(bignum num) {
  return num.used_digits == 0 || num.bigits[num.used_digits - 1] != 0;
}


void bignum_align(bignum* num, bignum other) {
  if (num->exponent > other.exponent) {
    // If "X" represents a "hidden" digit (by the exponent) then we are in the
    // following case (a == this, b == other):
    // a:  aaaaaaXXXX   or a:   aaaaaXXX
    // b:     bbbbbbX      b: bbbbbbbbXX
    // We replace some of the hidden digits (X) of a with 0 digits.
    // a:  aaaaaa000X   or a:   aaaaa0XX
    int zero_digits = num->exponent - other.exponent;
    bignum_ensure_capacity(num->used_digits + zero_digits);
    for (int i = num->used_digits - 1; i >= 0; --i) {
      num->bigits[i + zero_digits] = num->bigits[i];
    }
    for (int i = 0; i < zero_digits; ++i) {
      num->bigits[i] = 0;
    }
    num->used_digits += zero_digits;
    num->exponent -= zero_digits;
    ASSERT(num->used_digits >= 0);
    ASSERT(num->exponent >= 0);
  }
}


// Guaranteed to lie in one Bigit.
void bignum_assign_uint16(bignum* num, uint16_t value) {
  ASSERT(kBigitSize >= uint16_bit_size(value));
  bignum_zero(num);
  if (value == 0) return;

  bignum_ensure_capacity(1);
  num->bigits[0] = value;
  num->used_digits = 1;
}


void bignum_assign_uint64(bignum* num, uint64_t value) {
  const int kUInt64Size = 64;

  bignum_zero(num);
  if (value == 0) return;

  int needed_bigits = kUInt64Size / kBigitSize + 1;
  bignum_ensure_capacity(needed_bigits);
  for (int i = 0; i < needed_bigits; ++i) {
    num->bigits[i] = value & kBigitMask;
    value = value >> kBigitSize;
  }
  num->used_digits = needed_bigits;
  bignum_clamp(num);
}


void bignum_assign_bignum(bignum* num, const bignum other) {
  num->exponent = other.exponent;
  for (int i = 0; i < other.used_digits; ++i) {
    num->bigits[i] = other.bigits[i];
  }
  // Clear the excess digits (if there were any).
  for (int i = other.used_digits; i < num->used_digits; ++i) {
    num->bigits[i] = 0;
  }
  num->used_digits = other.used_digits;
}


static uint64_t bignum_read_uint64(const char* buffer,
                           int from,
                           int digits_to_read) {
  uint64_t result = 0;
  for (int i = from; i < from + digits_to_read; ++i) {
    int digit = buffer[i] - '0';
    ASSERT(0 <= digit && digit <= 9);
    result = result * 10 + digit;
  }
  return result;
}


void bignum_assign_decimal_string(bignum* num, const char* value) {
  // 2^64 = 18446744073709551616 > 10^19
  const int kMaxUint64DecimalDigits = 19;
  bignum_zero(num);
  int length = strlen(value);
  unsigned int pos = 0;
  // Let's just say that each digit needs 4 bits.
  while (length >= kMaxUint64DecimalDigits) {
    uint64_t digits = bignum_read_uint64(value, pos, kMaxUint64DecimalDigits);
    pos += kMaxUint64DecimalDigits;
    length -= kMaxUint64DecimalDigits;
    bignum_multiply_by_power_of_ten(num, kMaxUint64DecimalDigits);
    bignum_add_uint64(num, digits);
  }
  uint64_t digits = bignum_read_uint64(value, pos, length);
  bignum_multiply_by_power_of_ten(num, length);
  bignum_add_uint64(num, digits);
  bignum_clamp(num);
}


static int HexCharValue(char c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('a' <= c && c <= 'f') return 10 + c - 'a';
  ASSERT('A' <= c && c <= 'F');
  return 10 + c - 'A';
}


void bignum_assign_hex_string(bignum* num, const char* value) {
  bignum_zero(num);
  int length = strlen(value);

  int needed_bigits = length * 4 / kBigitSize + 1;
  bignum_ensure_capacity(needed_bigits);
  int string_index = length - 1;
  for (int i = 0; i < needed_bigits - 1; ++i) {
    // These bigits are guaranteed to be "full".
    Chunk current_bigit = 0;
    for (int j = 0; j < kBigitSize / 4; j++) {
      current_bigit += HexCharValue(value[string_index--]) << (j * 4);
    }
    num->bigits[i] = current_bigit;
  }
  num->used_digits = needed_bigits - 1;

  Chunk most_significant_bigit = 0;  // Could be = 0;
  for (int j = 0; j <= string_index; ++j) {
    most_significant_bigit <<= 4;
    most_significant_bigit += HexCharValue(value[j]);
  }
  if (most_significant_bigit != 0) {
    num->bigits[num->used_digits] = most_significant_bigit;
    num->used_digits++;
  }
  bignum_clamp(num);
}


void bignum_add_uint64(bignum* num, uint64_t operand) {
  if (operand == 0) return;
  bignum other = new_bignum();
  bignum_assign_uint64(&other, operand);
  bignum_add_bignum(num, other);
}


void bignum_add_bignum(bignum* num, const bignum other) {
  ASSERT(bignum_is_clamped(*num));
  ASSERT(bignum_is_clamped(other));

  // If this has a greater exponent than other append zero-bigits to this.
  // After this call num.exponent <= other.exponent.
  bignum_align(num, other);

  // There are two possibilities:
  //   aaaaaaaaaaa 0000  (where the 0s represent a's exponent)
  //     bbbbb 00000000
  //   ----------------
  //   ccccccccccc 0000
  // or
  //    aaaaaaaaaa 0000
  //  bbbbbbbbb 0000000
  //  -----------------
  //  cccccccccccc 0000
  // In both cases we might need a carry bigit.

  bignum_ensure_capacity(
    1 + max_int(bigit_length(*num), bigit_length(other)) - num->exponent
  );
  Chunk carry = 0;
  int bigit_pos = other.exponent - num->exponent;
  ASSERT(bigit_pos >= 0);
  for (int i = 0; i < other.used_digits; ++i) {
    Chunk sum = num->bigits[bigit_pos] + other.bigits[i] + carry;
    num->bigits[bigit_pos] = sum & kBigitMask;
    carry = sum >> kBigitSize;
    bigit_pos++;
  }

  while (carry != 0) {
    Chunk sum = num->bigits[bigit_pos] + carry;
    num->bigits[bigit_pos] = sum & kBigitMask;
    carry = sum >> kBigitSize;
    bigit_pos++;
  }
  num->used_digits = max_int(bigit_pos, num->used_digits);
  ASSERT(bignum_is_clamped(*num));
}


void bignum_subtract_bignum(bignum* num, const bignum other) {
  ASSERT(bignum_is_clamped(*num));
  ASSERT(bignum_is_clamped(other));
  // We require this to be bigger than other.
  ASSERT(bignum_less_equal(other, *num));

  bignum_align(num, other);

  int offset = other.exponent - num->exponent;
  Chunk borrow = 0;
  int i;
  for (i = 0; i < other.used_digits; ++i) {
    ASSERT((borrow == 0) || (borrow == 1));
    Chunk difference = num->bigits[i + offset] - other.bigits[i] - borrow;
    num->bigits[i + offset] = difference & kBigitMask;
    borrow = difference >> (kBigNumChunkSize - 1);
  }
  while (borrow != 0) {
    Chunk difference = num->bigits[i + offset] - borrow;
    num->bigits[i + offset] = difference & kBigitMask;
    borrow = difference >> (kBigNumChunkSize - 1);
    ++i;
  }
  bignum_clamp(num);
}


void bignum_subtract_times(bignum* num, bignum other, int factor) {
  ASSERT(num->exponent <= other.exponent);
  if (factor < 3) {
    for (int i = 0; i < factor; ++i) {
      bignum_subtract_bignum(num, other);
    }
    return;
  }
  Chunk borrow = 0;
  int exponent_diff = other.exponent - num->exponent;
  for (int i = 0; i < other.used_digits; ++i) {
    DoubleChunk product = (DoubleChunk)factor * other.bigits[i];
    DoubleChunk remove = borrow + product;
    Chunk difference = num->bigits[i + exponent_diff] - (remove & kBigitMask);
    num->bigits[i + exponent_diff] = difference & kBigitMask;
    borrow = (Chunk)((difference >> (kBigNumChunkSize - 1)) +
                                (remove >> kBigitSize));
  }
  for (int i = other.used_digits + exponent_diff; i < num->used_digits; ++i) {
    if (borrow == 0) return;
    Chunk difference = num->bigits[i] - borrow;
    num->bigits[i] = difference & kBigitMask;
    borrow = difference >> (kBigNumChunkSize - 1);
  }
  bignum_clamp(num);
}


void bignum_bigits_shift_left(bignum* num, int shift_amount) {
  ASSERT(shift_amount < kBigitSize);
  ASSERT(shift_amount >= 0);
  Chunk carry = 0;
  for (int i = 0; i < num->used_digits; ++i) {
    Chunk new_carry = num->bigits[i] >> (kBigitSize - shift_amount);
    num->bigits[i] = ((num->bigits[i] << shift_amount) + carry) & kBigitMask;
    carry = new_carry;
  }
  if (carry != 0) {
    num->bigits[num->used_digits] = carry;
    num->used_digits++;
  }
}


void bignum_shift_left(bignum* num, int shift_amount) {
  if (num->used_digits == 0) return;
  num->exponent += shift_amount / kBigitSize;
  int local_shift = shift_amount % kBigitSize;
  bignum_ensure_capacity(num->used_digits + 1);
  bignum_bigits_shift_left(num, local_shift);
}


void bignum_multiply_by_uint32(bignum* num, uint32_t factor) {
  if (factor == 1) return;
  if (factor == 0) {
    bignum_zero(num);
    return;
  }
  if (num->used_digits == 0) return;

  // The product of a bigit with the factor is of size kBigitSize + 32.
  // Assert that this number + 1 (for the carry) fits into double chunk.
  ASSERT(kBigNumDoubleChunkSize >= kBigitSize + 32 + 1);
  DoubleChunk carry = 0;
  for (int i = 0; i < num->used_digits; ++i) {
    DoubleChunk product = (DoubleChunk)factor * num->bigits[i] + carry;
    num->bigits[i] = (Chunk)(product & kBigitMask);
    carry = (product >> kBigitSize);
  }
  while (carry != 0) {
    bignum_ensure_capacity(num->used_digits + 1);
    num->bigits[num->used_digits] = carry & kBigitMask;
    num->used_digits++;
    carry >>= kBigitSize;
  }
}


void bignum_multiply_by_uint64(bignum* num, uint64_t factor) {
  if (factor == 1) return;
  if (factor == 0) {
    bignum_zero(num);
    return;
  }
  ASSERT(kBigitSize < 32);
  uint64_t carry = 0;
  uint64_t low = factor & 0xFFFFFFFF;
  uint64_t high = factor >> 32;
  for (int i = 0; i < num->used_digits; ++i) {
    uint64_t product_low = low * num->bigits[i];
    uint64_t product_high = high * num->bigits[i];
    uint64_t tmp = (carry & kBigitMask) + product_low;
    num->bigits[i] = tmp & kBigitMask;
    carry = (carry >> kBigitSize) + (tmp >> kBigitSize) +
        (product_high << (32 - kBigitSize));
  }
  while (carry != 0) {
    bignum_ensure_capacity(num->used_digits + 1);
    num->bigits[num->used_digits] = carry & kBigitMask;
    num->used_digits++;
    carry >>= kBigitSize;
  }
}


void bignum_multiply_by_power_of_ten(bignum* num, int exponent) {
  const uint64_t kFive27 = UINT64_2PART_C(0x6765c793, fa10079d);
  const uint16_t kFive1 = 5;
  const uint16_t kFive2 = kFive1 * 5;
  const uint16_t kFive3 = kFive2 * 5;
  const uint16_t kFive4 = kFive3 * 5;
  const uint16_t kFive5 = kFive4 * 5;
  const uint16_t kFive6 = kFive5 * 5;
  const uint32_t kFive7 = kFive6 * 5;
  const uint32_t kFive8 = kFive7 * 5;
  const uint32_t kFive9 = kFive8 * 5;
  const uint32_t kFive10 = kFive9 * 5;
  const uint32_t kFive11 = kFive10 * 5;
  const uint32_t kFive12 = kFive11 * 5;
  const uint32_t kFive13 = kFive12 * 5;
  const uint32_t kFive1_to_12[] =
      { kFive1, kFive2, kFive3, kFive4, kFive5, kFive6,
        kFive7, kFive8, kFive9, kFive10, kFive11, kFive12 };

  ASSERT(exponent >= 0);
  if (exponent == 0) return;
  if (num->used_digits == 0) return;

  // We shift by exponent at the end just before returning.
  int remaining_exponent = exponent;
  while (remaining_exponent >= 27) {
    bignum_multiply_by_uint64(num, kFive27);
    remaining_exponent -= 27;
  }
  while (remaining_exponent >= 13) {
    bignum_multiply_by_uint32(num, kFive13);
    remaining_exponent -= 13;
  }
  if (remaining_exponent > 0) {
    bignum_multiply_by_uint32(num, kFive1_to_12[remaining_exponent - 1]);
  }
  bignum_shift_left(num, exponent);
}


void bignum_times_10(bignum* num) {
  return bignum_multiply_by_uint32(num, 10);
}


void bignum_square(bignum* num) {
  ASSERT(bignum_is_clamped(*num));
  int product_length = 2 * num->used_digits;
  bignum_ensure_capacity(product_length);

  // Comba multiplication: compute each column separately.
  // Example: r = a2a1a0 * b2b1b0.
  //    r =  1    * a0b0 +
  //        10    * (a1b0 + a0b1) +
  //        100   * (a2b0 + a1b1 + a0b2) +
  //        1000  * (a2b1 + a1b2) +
  //        10000 * a2b2
  //
  // In the worst case we have to accumulate nb-digits products of digit*digit.
  //
  // Assert that the additional number of bits in a DoubleChunk are enough to
  // sum up used_digits of Bigit*Bigit.
  if ((1 << (2 * (kBigNumChunkSize - kBigitSize))) <= num->used_digits) {
    UNIMPLEMENTED();
  }
  DoubleChunk accumulator = 0;
  // First shift the digits so we don't overwrite them.
  int copy_offset = num->used_digits;
  for (int i = 0; i < num->used_digits; ++i) {
    num->bigits[copy_offset + i] = num->bigits[i];
  }
  // We have two loops to avoid some 'if's in the loop.
  for (int i = 0; i < num->used_digits; ++i) {
    // Process temporary digit i with power i.
    // The sum of the two indices must be equal to i.
    int bigit_index1 = i;
    int bigit_index2 = 0;
    // Sum all of the sub-products.
    while (bigit_index1 >= 0) {
      Chunk chunk1 = num->bigits[copy_offset + bigit_index1];
      Chunk chunk2 = num->bigits[copy_offset + bigit_index2];
      accumulator += (DoubleChunk)chunk1 * chunk2;
      bigit_index1--;
      bigit_index2++;
    }
    num->bigits[i] = (Chunk)accumulator & kBigitMask;
    accumulator >>= kBigitSize;
  }
  for (int i = num->used_digits; i < product_length; ++i) {
    int bigit_index1 = num->used_digits - 1;
    int bigit_index2 = i - bigit_index1;
    // Invariant: sum of both indices is again equal to i.
    // Inner loop runs 0 times on last iteration, emptying accumulator.
    while (bigit_index2 < num->used_digits) {
      Chunk chunk1 = num->bigits[copy_offset + bigit_index1];
      Chunk chunk2 = num->bigits[copy_offset + bigit_index2];
      accumulator += (DoubleChunk)chunk1 * chunk2;
      bigit_index1--;
      bigit_index2++;
    }
    // The overwritten num->bigits[i] will never be read in further loop
    // iterations, because bigit_index1 and bigit_index2 are always greater
    // than i - num->used_digits.
    num->bigits[i] = (Chunk)accumulator & kBigitMask;
    accumulator >>= kBigitSize;
  }
  // Since the result was guaranteed to lie inside the number the
  // accumulator must be 0 now.
  ASSERT(accumulator == 0);

  // Don't forget to update the used_digits and the exponent.
  num->used_digits = product_length;
  num->exponent *= 2;
  bignum_clamp(num);
}


void bignum_assign_power_uint16(
  bignum* num,
  uint16_t base,
  int power_exponent
) {
  ASSERT(base != 0);
  ASSERT(power_exponent >= 0);
  if (power_exponent == 0) {
    bignum_assign_uint16(num, 1);
    return;
  }
  bignum_zero(num);
  int shifts = 0;
  // We expect base to be in range 2-32, and most often to be 10.
  // It does not make much sense to implement different algorithms for counting
  // the bits.
  while ((base & 1) == 0) {
    base >>= 1;
    shifts++;
  }
  int bit_size = 0;
  int tmp_base = base;
  while (tmp_base != 0) {
    tmp_base >>= 1;
    bit_size++;
  }
  int final_size = bit_size * power_exponent;
  // 1 extra bigit for the shifting, and one for rounded final_size.
  bignum_ensure_capacity(final_size / kBigitSize + 2);

  // Left to Right exponentiation.
  int mask = 1;
  while (power_exponent >= mask) mask <<= 1;

  // The mask is now pointing to the bit above the most significant 1-bit of
  // power_exponent.
  // Get rid of first 1-bit;
  mask >>= 2;
  uint64_t this_value = base;

  bool delayed_multipliciation = false;
  const uint64_t max_32bits = 0xFFFFFFFF;
  while (mask != 0 && this_value <= max_32bits) {
    this_value = this_value * this_value;
    // Verify that there is enough space in this_value to perform the
    // multiplication.  The first bit_size bits must be 0.
    if ((power_exponent & mask) != 0) {
      uint64_t base_bits_mask =
          ~(((uint64_t)1 << (64 - bit_size)) - 1);
      bool high_bits_zero = (this_value & base_bits_mask) == 0;
      if (high_bits_zero) {
        this_value *= base;
      } else {
        delayed_multipliciation = true;
      }
    }
    mask >>= 1;
  }
  bignum_assign_uint64(num, this_value);
  if (delayed_multipliciation) {
    bignum_multiply_by_uint32(num, base);
  }

  // Now do the same thing as a bignum.
  while (mask != 0) {
    bignum_square(num);
    if ((power_exponent & mask) != 0) {
      bignum_multiply_by_uint32(num, base);
    }
    mask >>= 1;
  }

  // And finally add the saved shifts.
  bignum_shift_left(num, shifts * power_exponent);
}


// Precondition: this/other < 16bit.
uint16_t bignum_divide_modulo_int_bignum(bignum* num, const bignum other) {
  ASSERT(bignum_is_clamped(*num));
  ASSERT(bignum_is_clamped(other));
  ASSERT(other.used_digits > 0);

  // Easy case: if we have less digits than the divisor than the result is 0.
  // Note: this handles the case where this == 0, too.
  if (bigit_length(*num) < bigit_length(other)) {
    return 0;
  }

  bignum_align(num, other);

  uint16_t result = 0;

  // Start by removing multiples of 'other' until both numbers have the same
  // number of digits.
  while (bigit_length(*num) > bigit_length(other)) {
    // This naive approach is extremely inefficient if `this` divided by other
    // is big. This function is implemented for doubleToString where
    // the result should be small (less than 10).
    ASSERT(other.bigits[other.used_digits - 1] >= ((1 << kBigitSize) / 16));
    ASSERT(num->bigits[num->used_digits - 1] < 0x10000);
    // Remove the multiples of the first digit.
    // Example this = 23 and other equals 9. -> Remove 2 multiples.
    result += (uint16_t)(num->bigits[num->used_digits - 1]);
    bignum_subtract_times(num, other, num->bigits[num->used_digits - 1]);
  }

  ASSERT(bigit_length(*num) == bigit_length(other));

  // Both bignums are at the same length now.
  // Since other has more than 0 digits we know that the access to
  // num.bigits[num.used_digits - 1] is safe.
  Chunk this_bigit = num->bigits[num->used_digits - 1];
  Chunk other_bigit = other.bigits[other.used_digits - 1];

  if (other.used_digits == 1) {
    // Shortcut for easy (and common) case.
    int quotient = this_bigit / other_bigit;
    num->bigits[num->used_digits - 1] = this_bigit - other_bigit * quotient;
    ASSERT(quotient < 0x10000);
    result += (uint16_t)quotient;
    bignum_clamp(num);
    return result;
  }

  int division_estimate = this_bigit / (other_bigit + 1);
  ASSERT(division_estimate < 0x10000);
  result += (uint16_t)division_estimate;
  bignum_subtract_times(num, other, division_estimate);

  if (other_bigit * (division_estimate + 1) > this_bigit) {
    // No need to even try to subtract. Even if other's remaining digits were 0
    // another subtraction would be too much.
    return result;
  }

  while (bignum_less_equal(other, *num)) {
    bignum_subtract_bignum(num, other);
    result++;
  }
  return result;
}


static int chunk_size_in_hex_chars(Chunk number) {
  ASSERT(number > 0);
  int result = 0;
  while (number != 0) {
    number >>= 4;
    result++;
  }
  return result;
}


static char HexCharOfValue(int value) {
  ASSERT(0 <= value && value <= 16);
  if (value < 10) return (char)(value + '0');
  return (char)(value - 10 + 'A');
}


bool bignum_to_hex_string(bignum num, char* buffer, int buffer_size) {
  ASSERT(bignum_is_clamped(num));
  // Each bigit must be printable as separate hex-character.
  ASSERT(kBigitSize % 4 == 0);
  const int kHexCharsPerBigit = kBigitSize / 4;

  if (num.used_digits == 0) {
    if (buffer_size < 2) return false;
    buffer[0] = '0';
    buffer[1] = '\0';
    return true;
  }
  // We add 1 for the terminating '\0' character.
  int needed_chars = (bigit_length(num) - 1) * kHexCharsPerBigit +
      chunk_size_in_hex_chars(num.bigits[num.used_digits - 1]) + 1;
  if (needed_chars > buffer_size) return false;
  int string_index = needed_chars - 1;
  buffer[string_index--] = '\0';
  for (int i = 0; i < num.exponent; ++i) {
    for (int j = 0; j < kHexCharsPerBigit; ++j) {
      buffer[string_index--] = '0';
    }
  }
  for (int i = 0; i < num.used_digits - 1; ++i) {
    Chunk current_bigit = num.bigits[i];
    for (int j = 0; j < kHexCharsPerBigit; ++j) {
      buffer[string_index--] = HexCharOfValue(current_bigit & 0xF);
      current_bigit >>= 4;
    }
  }
  // And finally the last bigit.
  Chunk most_significant_bigit = num.bigits[num.used_digits - 1];
  while (most_significant_bigit != 0) {
    buffer[string_index--] = HexCharOfValue(most_significant_bigit & 0xF);
    most_significant_bigit >>= 4;
  }
  return true;
}


Chunk bignum_bigit_at(bignum num, int index) {
  if (index >= bigit_length(num)) return 0;
  if (index < num.exponent) return 0;
  return num.bigits[index - num.exponent];
}


int bignum_compare(bignum a, bignum b) {
  ASSERT(bignum_is_clamped(a));
  ASSERT(bignum_is_clamped(b));
  int bigit_length_a = bigit_length(a);
  int bigit_length_b = bigit_length(b);
  if (bigit_length_a < bigit_length_b) return -1;
  if (bigit_length_a > bigit_length_b) return +1;
  for (int i = bigit_length_a - 1; i >= min_int(a.exponent, b.exponent); --i) {
    Chunk bigit_a = bignum_bigit_at(a, i);
    Chunk bigit_b = bignum_bigit_at(b, i);
    if (bigit_a < bigit_b) return -1;
    if (bigit_a > bigit_b) return +1;
    // Otherwise they are equal up to this digit. Try the next digit.
  }
  return 0;
}

bool bignum_equal(bignum a, bignum b) {
  return bignum_compare(a, b) == 0;
}
bool bignum_less_equal(bignum a, bignum b) {
  return bignum_compare(a, b) <= 0;
}
bool bignum_less(bignum a, bignum b) {
  return bignum_compare(a, b) < 0;
}


int bignum_plus_compare(bignum a, bignum b, bignum c) {
  ASSERT(bignum_is_clamped(a));
  ASSERT(bignum_is_clamped(b));
  ASSERT(bignum_is_clamped(c));
  if (bigit_length(a) < bigit_length(b)) {
    return bignum_plus_compare(b, a, c);
  }
  if (bigit_length(a) + 1 < bigit_length(c)) return -1;
  if (bigit_length(a) > bigit_length(c)) return +1;
  // The exponent encodes 0-bigits. So if there are more 0-digits in 'a' than
  // 'b' has digits, then the bigit-length of 'a'+'b' must be equal to the one
  // of 'a'.
  if (a.exponent >= bigit_length(b) && bigit_length(a) < bigit_length(c)) {
    return -1;
  }

  Chunk borrow = 0;
  // Starting at min_exponent all digits are == 0. So no need to compare them.
  int min_exponent = min_int(min_int(a.exponent, b.exponent), c.exponent);
  for (int i = bigit_length(c) - 1; i >= min_exponent; --i) {
    Chunk chunk_a = bignum_bigit_at(a, i);
    Chunk chunk_b = bignum_bigit_at(b, i);
    Chunk chunk_c = bignum_bigit_at(c, i);
    Chunk sum = chunk_a + chunk_b;
    if (sum > chunk_c + borrow) {
      return +1;
    } else {
      borrow = chunk_c + borrow - sum;
      if (borrow > 1) return -1;
      borrow <<= kBigitSize;
    }
  }
  if (borrow == 0) return 0;
  return -1;
}

bool bignum_plus_equal(bignum a, bignum b, bignum c) {
  return bignum_plus_compare(a, b, c) == 0;
}
// Returns a + b <= c
bool bignum_plus_less_equal(bignum a, bignum b, bignum c) {
  return bignum_plus_compare(a, b, c) <= 0;
}
// Returns a + b < c
bool bignum_plus_less(bignum a, bignum b, bignum c) {
  return bignum_plus_compare(a, b, c) < 0;
}
