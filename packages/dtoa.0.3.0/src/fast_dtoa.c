/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>

#include "cached_powers.h"
#include "fast_dtoa.h"
#include "ieee.h"

// The minimal and maximal target exponent define the range of w's binary
// exponent, where 'w' is the result of multiplying the input by a cached power
// of ten.
//
// A different range might be chosen on a different platform, to optimize digit
// generation, but a smaller range requires more powers of ten to be cached.
static const int kMinimalTargetExponent = -60;
static const int kMaximalTargetExponent = -32;


// Adjusts the last digit of the generated number, and screens out generated
// solutions that may be inaccurate. A solution may be inaccurate if it is
// outside the safe interval, or if we cannot prove that it is closer to the
// input than a neighboring representation of the same length.
//
// Input: * buffer containing the digits of too_high / 10^kappa
//        * the buffer's length
//        * distance_too_high_w == (too_high - w).f() * unit
//        * unsafe_interval == (too_high - too_low).f() * unit
//        * rest = (too_high - buffer * 10^kappa).f() * unit
//        * ten_kappa = 10^kappa * unit
//        * unit = the common multiplier
// Output: returns true if the buffer is guaranteed to contain the closest
//    representable number to the input.
//  Modifies the generated digits in the buffer to approach (round towards) w.
static bool round_weed(char* buffer,
                       int length,
                       uint64_t distance_too_high_w,
                       uint64_t unsafe_interval,
                       uint64_t rest,
                       uint64_t ten_kappa,
                       uint64_t unit) {
  uint64_t small_distance = distance_too_high_w - unit;
  uint64_t big_distance = distance_too_high_w + unit;
  // Let w_low  = too_high - big_distance, and
  //     w_high = too_high - small_distance.
  // Note: w_low < w < w_high
  //
  // The real w (* unit) must lie somewhere inside the interval
  // ]w_low; w_high[ (often written as "(w_low; w_high)")

  // Basically the buffer currently contains a number in the unsafe interval
  // ]too_low; too_high[ with too_low < w < too_high
  //
  //  too_high - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //                     ^v 1 unit            ^      ^                 ^      ^
  //  boundary_high ---------------------     .      .                 .      .
  //                     ^v 1 unit            .      .                 .      .
  //   - - - - - - - - - - - - - - - - - - -  +  - - + - - - - - -     .      .
  //                                          .      .         ^       .      .
  //                                          .  big_distance  .       .      .
  //                                          .      .         .       .    rest
  //                              small_distance     .         .       .      .
  //                                          v      .         .       .      .
  //  w_high - - - - - - - - - - - - - - - - - -     .         .       .      .
  //                     ^v 1 unit                   .         .       .      .
  //  w ----------------------------------------     .         .       .      .
  //                     ^v 1 unit                   v         .       .      .
  //  w_low  - - - - - - - - - - - - - - - - - - - - -         .       .      .
  //                                                           .       .      v
  //  buffer --------------------------------------------------+-------+--------
  //                                                           .       .
  //                                                  safe_interval    .
  //                                                           v       .
  //   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     .
  //                     ^v 1 unit                                     .
  //  boundary_low -------------------------                     unsafe_interval
  //                     ^v 1 unit                                     v
  //  too_low  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  //
  // Note that the value of buffer could lie anywhere inside the range too_low
  // to too_high.
  //
  // boundary_low, boundary_high and w are approximations of the real boundaries
  // and v (the input number). They are guaranteed to be precise up to one unit.
  // In fact the error is guaranteed to be strictly less than one unit.
  //
  // Anything that lies outside the unsafe interval is guaranteed not to round
  // to v when read again.
  // Anything that lies inside the safe interval is guaranteed to round to v
  // when read again.
  // If the number inside the buffer lies inside the unsafe interval but not
  // inside the safe interval then we simply do not know and bail out (returning
  // false).
  //
  // Similarly we have to take into account the imprecision of 'w' when finding
  // the closest representation of 'w'. If we have two potential
  // representations, and one is closer to both w_low and w_high, then we know
  // it is closer to the actual value v.
  //
  // By generating the digits of too_high we got the largest (closest to
  // too_high) buffer that is still in the unsafe interval. In the case where
  // w_high < buffer < too_high we try to decrement the buffer.
  // This way the buffer approaches (rounds towards) w.
  // There are 3 conditions that stop the decrementation process:
  //   1) the buffer is already below w_high
  //   2) decrementing the buffer would make it leave the unsafe interval
  //   3) decrementing the buffer would yield a number below w_high and farther
  //      away than the current number. In other words:
  //              (buffer{-1} < w_high) && w_high - buffer{-1} > buffer - w_high
  // Instead of using the buffer directly we use its distance to too_high.
  // Conceptually rest ~= too_high - buffer
  // We need to do the following tests in this order to avoid over- and
  // underflows.
  ASSERT(rest <= unsafe_interval);
  while (rest < small_distance &&  // Negated condition 1
         unsafe_interval - rest >= ten_kappa &&  // Negated condition 2
         (rest + ten_kappa < small_distance ||  // buffer{-1} > w_high
          small_distance - rest >= rest + ten_kappa - small_distance)) {
    buffer[length - 1]--;
    rest += ten_kappa;
  }

  // We have approached w+ as much as possible. We now test if approaching w-
  // would require changing the buffer. If yes, then we have two possible
  // representations close to w, but we cannot decide which one is closer.
  if (rest < big_distance &&
      unsafe_interval - rest >= ten_kappa &&
      (rest + ten_kappa < big_distance ||
       big_distance - rest > rest + ten_kappa - big_distance)) {
    return false;
  }

  // Weeding test.
  //   The safe interval is [too_low + 2 ulp; too_high - 2 ulp]
  //   Since too_low = too_high - unsafe_interval this is equivalent to
  //      [too_high - unsafe_interval + 4 ulp; too_high - 2 ulp]
  //   Conceptually we have: rest ~= too_high - buffer
  return (2 * unit <= rest) && (rest <= unsafe_interval - 4 * unit);
}


// Returns the biggest power of ten that is less than or equal to the given
// number. We furthermore receive the maximum number of bits 'number' has.
//
// Returns power == 10^(exponent_plus_one-1) such that
//    power <= number < power * 10.
// If number_bits == 0 then 0^(0-1) is returned.
// The number of bits must be <= 32.
// Precondition: number < (1 << (number_bits + 1)).

// Inspired by the method for finding an integer log base 10 from here:
// http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog10
static unsigned int const kSmallPowersOfTen[] =
    {0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
     1000000000};

static void biggest_power_ten(uint32_t number,
                              int number_bits,
                              uint32_t* power,
                              int* exponent_plus_one) {
  ASSERT(number < (1u << (number_bits + 1)));
  // 1233/4096 is approximately 1/lg(10).
  int exponent_plus_one_guess = ((number_bits + 1) * 1233 >> 12);
  // We increment to skip over the first entry in the kPowersOf10 table.
  // Note: kPowersOf10[i] == 10^(i-1).
  exponent_plus_one_guess++;
  // We don't have any guarantees that 2^number_bits <= number.
  if (number < kSmallPowersOfTen[exponent_plus_one_guess]) {
    exponent_plus_one_guess--;
  }
  *power = kSmallPowersOfTen[exponent_plus_one_guess];
  *exponent_plus_one = exponent_plus_one_guess;
}

// Generates the digits of input number w.
// w is a floating-point number (DiyFp), consisting of a significand and an
// exponent. Its exponent is bounded by kMinimalTargetExponent and
// kMaximalTargetExponent.
//       Hence -60 <= w.e <= -32.
//
// Returns false if it fails, in which case the generated digits in the buffer
// should not be used.
// Preconditions:
//  * low, w and high are correct up to 1 ulp (unit in the last place). That
//    is, their error must be less than a unit of their last digits.
//  * low.e == w.e == high.e
//  * low < w < high, and taking into account their error: low~ <= high~
//  * kMinimalTargetExponent <= w.e <= kMaximalTargetExponent
// Postconditions: returns false if procedure fails.
//   otherwise:
//     * buffer is not null-terminated, but len contains the number of digits.
//     * buffer contains the shortest possible decimal digit-sequence
//       such that LOW < buffer * 10^kappa < HIGH, where LOW and HIGH are the
//       correct values of low and high (without their error).
//     * if more than one decimal representation gives the minimal number of
//       decimal digits then the one closest to W (where W is the correct value
//       of w) is chosen.
// Remark: this procedure takes into account the imprecision of its input
//   numbers. If the precision is not enough to guarantee all the postconditions
//   then false is returned. This usually happens rarely (~0.5%).
//
// Say, for the sake of example, that
//   w.e == -48, and w.f == 0x1234567890abcdef
// w's value can be computed by w.f * 2^w.e
// We can obtain w's integral digits by simply shifting w.f by -w.e.
//  -> w's integral part is 0x1234
//  w's fractional part is therefore 0x567890abcdef.
// Printing w's integral part is easy (simply print 0x1234 in decimal).
// In order to print its fraction we repeatedly multiply the fraction by 10 and
// get each digit. Example the first digit after the point would be computed by
//   (0x567890abcdef * 10) >> 48. -> 3
// The whole thing becomes slightly more complicated because we want to stop
// once we have enough digits. That is, once the digits inside the buffer
// represent 'w' we can stop. Everything inside the interval low - high
// represents w. However we have to pay attention to low, high and w's
// imprecision.
static bool digit_gen(diy_fp low,
                      diy_fp w,
                      diy_fp high,
                      char* buffer,
                      int* length,
                      int* kappa) {
  ASSERT(low.e == w.e && w.e == high.e);
  ASSERT(low.f + 1 <= high.f - 1);
  ASSERT(kMinimalTargetExponent <= w.e && w.e <= kMaximalTargetExponent);
  // low, w and high are imprecise, but by less than one ulp (unit in the last
  // place).
  // If we remove (resp. add) 1 ulp from low (resp. high) we are certain that
  // the new numbers are outside of the interval we want the final
  // representation to lie in.
  // Inversely adding (resp. removing) 1 ulp from low (resp. high) would yield
  // numbers that are certain to lie in the interval. We will use this fact
  // later on.
  // We will now start by generating the digits within the uncertain
  // interval. Later we will weed out representations that lie outside the safe
  // interval and thus _might_ lie outside the correct interval.
  uint64_t unit = 1;
  diy_fp too_low = { low.f - unit, low.e };
  diy_fp too_high = { high.f + unit, high.e };
  // too_low and too_high are guaranteed to lie outside the interval we want the
  // generated number in.
  diy_fp unsafe_interval = diy_fp_minus(too_high, too_low);
  // We now cut the input number into two parts: the integral digits and the
  // fractionals. We will not write any decimal separator though, but adapt
  // kappa instead.
  // Reminder: we are currently computing the digits (stored inside the buffer)
  // such that:   too_low < buffer * 10^kappa < too_high
  // We use too_high for the digit_generation and stop as soon as possible.
  // If we stop early we effectively round down.
  diy_fp one = { (uint64_t)(1) << -w.e, w.e };
  // Division by one is a shift.
  uint32_t integrals = (uint32_t)(too_high.f >> -one.e);
  // Modulo by one is an and.
  uint64_t fractionals = too_high.f & (one.f - 1);
  uint32_t divisor;
  int divisor_exponent_plus_one;
  biggest_power_ten(integrals, kDiyFpSignificandSize - (-one.e),
                    &divisor, &divisor_exponent_plus_one);
  *kappa = divisor_exponent_plus_one;
  *length = 0;
  // Loop invariant: buffer = too_high / 10^kappa  (integer division)
  // The invariant holds for the first iteration: kappa has been initialized
  // with the divisor exponent + 1. And the divisor is the biggest power of ten
  // that is smaller than integrals.
  while (*kappa > 0) {
    int digit = integrals / divisor;
    ASSERT(digit <= 9);
    buffer[*length] = (char)('0' + digit);
    (*length)++;
    integrals %= divisor;
    (*kappa)--;
    // Note that kappa now equals the exponent of the divisor and that the
    // invariant thus holds again.
    uint64_t rest = ((uint64_t)(integrals) << -one.e) + fractionals;
    // Invariant: too_high = buffer * 10^kappa + DiyFp(rest, one.e)
    // Reminder: unsafe_interval.e == one.e
    if (rest < unsafe_interval.f) {
      // Rounding down (by not emitting the remaining digits) yields a number
      // that lies within the unsafe interval.
      return round_weed(buffer, *length, diy_fp_minus(too_high, w).f,
                        unsafe_interval.f, rest,
                        (uint64_t)(divisor) << -one.e, unit);
    }
    divisor /= 10;
  }

  // The integrals have been generated. We are at the point of the decimal
  // separator. In the following loop we simply multiply the remaining digits by
  // 10 and divide by one. We just need to pay attention to multiply associated
  // data (like the interval or 'unit'), too.
  // Note that the multiplication by 10 does not overflow, because w.e >= -60
  // and thus one.e >= -60.
  ASSERT(one.e >= -60);
  ASSERT(fractionals < one.f);
  ASSERT(UINT64_2PART_C(0xFFFFFFFF, FFFFFFFF) / 10 >= one.f);
  for (;;) {
    fractionals *= 10;
    unit *= 10;
    unsafe_interval.f = unsafe_interval.f * 10;
    // Integer division by one.
    int digit = (int)(fractionals >> -one.e);
    ASSERT(digit <= 9);
    buffer[*length] = (char)('0' + digit);
    (*length)++;
    fractionals &= one.f - 1;  // Modulo by one.
    (*kappa)--;
    if (fractionals < unsafe_interval.f) {
      return round_weed(buffer, *length, diy_fp_minus(too_high, w).f * unit,
                        unsafe_interval.f, fractionals, one.f, unit);
    }
  }
}

// Provides a decimal representation of v.
// Returns true if it succeeds, otherwise the result cannot be trusted.
// There will be *length digits inside the buffer (not null-terminated).
// If the function returns true then
//        v == (double) (buffer * 10^decimal_exponent).
// The digits in the buffer are the shortest representation possible: no
// 0.09999999999999999 instead of 0.1. The shorter representation will even be
// chosen even if the longer one would be closer to v.
// The last digit will be closest to the actual v. That is, even if several
// digits might correctly yield 'v' when read again, the closest will be
// computed.
static bool grisu3(double v,
                   FastDtoaMode mode,
                   char* buffer,
                   int* length,
                   int* decimal_exponent) {
  diy_fp w = double_as_normalized_diy_fp(v);

  // boundary_minus and boundary_plus are the boundaries between v and its
  // closest floating-point neighbors. Any number strictly between
  // boundary_minus and boundary_plus will round to v when convert to a double.
  // Grisu3 will never output representations that lie exactly on a boundary.
  diy_fp boundary_minus, boundary_plus;
  if (mode == FAST_DTOA_SHORTEST) {
    double_normalized_boundaries(v, &boundary_minus, &boundary_plus);
  } else {
    ASSERT(mode == FAST_DTOA_SHORTEST_SINGLE);
    // float single_v = (float)(v);
    // Single(single_v).NormalizedBoundaries(&boundary_minus, &boundary_plus);
  }
  ASSERT(boundary_plus.e == w.e);
  diy_fp ten_mk;  // Cached power of ten: 10^-k
  int mk;        // -k
  int ten_mk_minimal_binary_exponent =
     kMinimalTargetExponent - (w.e + kDiyFpSignificandSize);
  int ten_mk_maximal_binary_exponent =
     kMaximalTargetExponent - (w.e + kDiyFpSignificandSize);
  cached_power_for_binary_exponent_range(
      ten_mk_minimal_binary_exponent,
      ten_mk_maximal_binary_exponent,
      &ten_mk, &mk);
  ASSERT((kMinimalTargetExponent <= w.e + ten_mk.e + kDiyFpSignificandSize) &&
         (kMaximalTargetExponent >= w.e + ten_mk.e + kDiyFpSignificandSize));
  // Note that ten_mk is only an approximation of 10^-k. A diy_fp only contains
  // a 64 bit significand and ten_mk is thus only precise up to 64 bits.

  // The diy_fp_multiply procedure rounds its result, and ten_mk is approximated
  // too. The variable scaled_w (as well as scaled_boundary_minus/plus) are now
  // off by a small amount.
  // In fact: scaled_w - w*10^k < 1ulp (unit in the last place) of scaled_w.
  // In other words: let f = scaled_w.f and e = scaled_w.e, then
  //           (f-1) * 2^e < w*10^k < (f+1) * 2^e
  diy_fp scaled_w = diy_fp_multiply(w, ten_mk);
  ASSERT(scaled_w.e == boundary_plus.e + ten_mk.e + kDiyFpSignificandSize);
  // In theory it would be possible to avoid some recomputations by computing
  // the difference between w and boundary_minus/plus (a power of 2) and to
  // compute scaled_boundary_minus/plus by subtracting/adding from
  // scaled_w. However the code becomes much less readable and the speed
  // enhancements are not terriffic.
  diy_fp scaled_boundary_minus = diy_fp_multiply(boundary_minus, ten_mk);
  diy_fp scaled_boundary_plus  = diy_fp_multiply(boundary_plus,  ten_mk);

  // digit_gen will generate the digits of scaled_w. Therefore we have
  // v == (double) (scaled_w * 10^-mk).
  // Set decimal_exponent == -mk and pass it to digit_gen. If scaled_w is not an
  // integer than it will be updated. For instance if scaled_w == 1.23 then
  // the buffer will be filled with "123" und the decimal_exponent will be
  // decreased by 2.
  int kappa;
  bool result = digit_gen(scaled_boundary_minus, scaled_w, scaled_boundary_plus,
                          buffer, length, &kappa);
  *decimal_exponent = -mk + kappa;
  return result;
}

bool fast_dtoa(double v,
               FastDtoaMode mode,
               int requested_digits,
               char* buffer,
               int* length,
               int* decimal_point) {
  ASSERT(v > 0);
  ASSERT(!double_is_special(v));

  bool result = false;
  int decimal_exponent = 0;
  switch (mode) {
    case FAST_DTOA_SHORTEST:
    case FAST_DTOA_SHORTEST_SINGLE:
      result = grisu3(v, mode, buffer, length, &decimal_exponent);
      break;
    case FAST_DTOA_PRECISION:
      // result = Grisu3Counted(v, requested_digits,
      //                        buffer, length, &decimal_exponent);
      break;
    // default:
    //   UNREACHABLE();
  }
  if (result) {
    *decimal_point = *length + decimal_exponent;
    buffer[*length] = '\0';
  }
  return result;
}
