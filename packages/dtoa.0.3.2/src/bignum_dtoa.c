/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <math.h>

#include "bignum_dtoa.h"

#include "bignum.h"
#include "ieee.h"

static int NormalizedExponent(uint64_t significand, int exponent) {
  ASSERT(significand != 0);
  while ((significand & kDoubleHiddenBit) == 0) {
    significand = significand << 1;
    exponent = exponent - 1;
  }
  return exponent;
}


// Forward declarations:
// Returns an estimation of k such that 10^(k-1) <= v < 10^k.
static int EstimatePower(int exponent);
// Computes v / 10^estimated_power exactly, as a ratio of two bignums, numerator
// and denominator.
static void InitialScaledStartValues(uint64_t significand,
                                     int exponent,
                                     bool lower_boundary_is_closer,
                                     int estimated_power,
                                     bool need_boundary_deltas,
                                     bignum* numerator,
                                     bignum* denominator,
                                     bignum* delta_minus,
                                     bignum* delta_plus);
// Multiplies numerator/denominator so that its values lies in the range 1-10.
// Returns decimal_point s.t.
//  v = numerator'/denominator' * 10^(decimal_point-1)
//     where numerator' and denominator' are the values of numerator and
//     denominator after the call to this function.
static void FixupMultiply10(int estimated_power, bool is_even,
                            int* decimal_point,
                            bignum* numerator, bignum* denominator,
                            bignum* delta_minus, bignum* delta_plus);
// Generates digits from the left to the right and stops when the generated
// digits yield the shortest decimal representation of v.
static void GenerateShortestDigits(bignum* numerator, bignum* denominator,
                                   bignum* delta_minus, bignum* delta_plus,
                                   bool is_even,
                                   char* buffer, int* length);
// Generates 'requested_digits' after the decimal point.
static void BignumToFixed(int requested_digits, int* decimal_point,
                          bignum* numerator, bignum* denominator,
                          char*(buffer), int* length);
// Generates 'count' digits of numerator/denominator.
// Once 'count' digits have been produced rounds the result depending on the
// remainder (remainders of exactly .5 round upwards). Might update the
// decimal_point when rounding up (for example for 0.9999).
static void GenerateCountedDigits(int count, int* decimal_point,
                                  bignum* numerator, bignum* denominator,
                                  char*(buffer), int* length);


void bignum_dtoa(
  double v,
  BignumDtoaMode mode,
  int requested_digits,
  char* buffer,
  int* length,
  int* decimal_point
) {
  ASSERT(v > 0);
  ASSERT(!double_is_special(v));
  uint64_t significand;
  int exponent;
  bool lower_boundary_is_closer;
  if (mode == BIGNUM_DTOA_SHORTEST_SINGLE) {
    // TODO
    // float f = (float)v;
    // ASSERT(f == v);
    // significand = Single(f).Significand();
    // exponent = Single(f).Exponent();
    // lower_boundary_is_closer = Single(f).LowerBoundaryIsCloser();
    UNIMPLEMENTED();
  } else {
    significand = double_significand(v);
    exponent = double_exponent(v);
    lower_boundary_is_closer = double_lower_boundary_is_closer(v);
  }
  bool need_boundary_deltas =
      (mode == BIGNUM_DTOA_SHORTEST || mode == BIGNUM_DTOA_SHORTEST_SINGLE);

  bool is_even = (significand & 1) == 0;
  int normalized_exponent = NormalizedExponent(significand, exponent);
  // estimated_power might be too low by 1.
  int estimated_power = EstimatePower(normalized_exponent);

  // Shortcut for Fixed.
  // The requested digits correspond to the digits after the point. If the
  // number is much too small, then there is no need in trying to get any
  // digits.
  if (mode == BIGNUM_DTOA_FIXED && -estimated_power - 1 > requested_digits) {
    buffer[0] = '\0';
    *length = 0;
    // Set decimal-point to -requested_digits. This is what Gay does.
    // Note that it should not have any effect anyways since the string is
    // empty.
    *decimal_point = -requested_digits;
    return;
  }

  bignum numerator = new_bignum();
  bignum denominator = new_bignum();
  bignum delta_minus = new_bignum();
  bignum delta_plus = new_bignum();
  // Make sure the bignum can grow large enough. The smallest double equals
  // 4e-324. In this case the denominator needs fewer than 324*4 binary digits.
  // The maximum double is 1.7976931348623157e308 which needs fewer than
  // 308*4 binary digits.
  ASSERT(kBigNumMaxSignificantBits >= 324*4);

  InitialScaledStartValues(significand, exponent, lower_boundary_is_closer,
                           estimated_power, need_boundary_deltas,
                           &numerator, &denominator,
                           &delta_minus, &delta_plus);

  // We now have v = (numerator / denominator) * 10^estimated_power.
  FixupMultiply10(estimated_power, is_even, decimal_point,
                  &numerator, &denominator,
                  &delta_minus, &delta_plus);

  // We now have v = (numerator / denominator) * 10^(decimal_point-1), and
  //  1 <= (numerator + delta_plus) / denominator < 10
  switch (mode) {
    case BIGNUM_DTOA_SHORTEST:
    case BIGNUM_DTOA_SHORTEST_SINGLE:
      GenerateShortestDigits(&numerator, &denominator,
                             &delta_minus, &delta_plus,
                             is_even, buffer, length);
      break;
    case BIGNUM_DTOA_FIXED:
      BignumToFixed(requested_digits, decimal_point,
                    &numerator, &denominator,
                    buffer, length);
      break;
    case BIGNUM_DTOA_PRECISION:
      GenerateCountedDigits(requested_digits, decimal_point,
                            &numerator, &denominator,
                            buffer, length);
      break;
    default:
      UNREACHABLE();
  }
  buffer[*length] = '\0';
}


// The procedure starts generating digits from the left to the right and stops
// when the generated digits yield the shortest decimal representation of v. A
// decimal representation of v is a number lying closer to v than to any other
// double, so it converts to v when read.
//
// This is true if d, the decimal representation, is between m- and m+, the
// upper and lower boundaries. d must be strictly between them if !is_even.
//           m- := (numerator - delta_minus) / denominator
//           m+ := (numerator + delta_plus) / denominator
//
// Precondition: 0 <= (numerator+delta_plus) / denominator < 10.
//   If 1 <= (numerator+delta_plus) / denominator < 10 then no leading 0 digit
//   will be produced. This should be the standard precondition.
static void GenerateShortestDigits(bignum* numerator, bignum* denominator,
                                   bignum* delta_minus, bignum* delta_plus,
                                   bool is_even,
                                   char* buffer, int* length) {
  // Small optimization: if delta_minus and delta_plus are the same just reuse
  // one of the two bignums.
  if (bignum_equal(*delta_minus, *delta_plus)) {
    delta_plus = delta_minus;
  }
  *length = 0;
  for (;;) {
    uint16_t digit;
    digit = bignum_divide_modulo_int_bignum(numerator, *denominator);
    ASSERT(digit <= 9);  // digit is a uint16_t and therefore always positive.
    // digit = numerator / denominator (integer division).
    // numerator = numerator % denominator.
    buffer[(*length)++] = (char)(digit + '0');

    // Can we stop already?
    // If the remainder of the division is less than the distance to the lower
    // boundary we can stop. In this case we simply round down (discarding the
    // remainder).
    // Similarly we test if we can round up (using the upper boundary).
    bool in_delta_room_minus;
    bool in_delta_room_plus;
    if (is_even) {
      in_delta_room_minus = bignum_less_equal(*numerator, *delta_minus);
    } else {
      in_delta_room_minus = bignum_less(*numerator, *delta_minus);
    }
    if (is_even) {
      in_delta_room_plus =
          bignum_plus_compare(*numerator, *delta_plus, *denominator) >= 0;
    } else {
      in_delta_room_plus =
          bignum_plus_compare(*numerator, *delta_plus, *denominator) > 0;
    }
    if (!in_delta_room_minus && !in_delta_room_plus) {
      // Prepare for next iteration.
      bignum_times_10(numerator);
      bignum_times_10(delta_minus);
      // We optimized delta_plus to be equal to delta_minus (if they share the
      // same value). So don't multiply delta_plus if they point to the same
      // object.
      if (delta_minus != delta_plus) {
        bignum_times_10(delta_plus);
      }
    } else if (in_delta_room_minus && in_delta_room_plus) {
      // Let's see if 2*numerator < denominator.
      // If yes, then the next digit would be < 5 and we can round down.
      int compare = bignum_plus_compare(*numerator, *numerator, *denominator);
      if (compare < 0) {
        // Remaining digits are less than .5. -> Round down (== do nothing).
      } else if (compare > 0) {
        // Remaining digits are more than .5 of denominator. -> Round up.
        // Note that the last digit could not be a '9' as otherwise the whole
        // loop would have stopped earlier.
        // We still have an assert here in case the preconditions were not
        // satisfied.
        ASSERT(buffer[(*length) - 1] != '9');
        buffer[(*length) - 1]++;
      } else {
        // Halfway case.
        // TODO(floitsch): need a way to solve half-way cases.
        //   For now let's round towards even (since this is what Gay seems to
        //   do).

        if ((buffer[(*length) - 1] - '0') % 2 == 0) {
          // Round down => Do nothing.
        } else {
          ASSERT(buffer[(*length) - 1] != '9');
          buffer[(*length) - 1]++;
        }
      }
      return;
    } else if (in_delta_room_minus) {
      // Round down (== do nothing).
      return;
    } else {  // in_delta_room_plus
      // Round up.
      // Note again that the last digit could not be '9' since this would have
      // stopped the loop earlier.
      // We still have an ASSERT here, in case the preconditions were not
      // satisfied.
      ASSERT(buffer[(*length) -1] != '9');
      buffer[(*length) - 1]++;
      return;
    }
  }
}


// Let v = numerator / denominator < 10.
// Then we generate 'count' digits of d = x.xxxxx... (without the decimal point)
// from left to right. Once 'count' digits have been produced we decide wether
// to round up or down. Remainders of exactly .5 round upwards. Numbers such
// as 9.999999 propagate a carry all the way, and change the
// exponent (decimal_point), when rounding upwards.
static void GenerateCountedDigits(int count, int* decimal_point,
                                  bignum* numerator, bignum* denominator,
                                  char* buffer, int* length) {
  ASSERT(count >= 0);
  for (int i = 0; i < count - 1; ++i) {
    uint16_t digit;
    digit = bignum_divide_modulo_int_bignum(numerator, *denominator);
    ASSERT(digit <= 9);  // digit is a uint16_t and therefore always positive.
    // digit = numerator / denominator (integer division).
    // numerator = numerator % denominator.
    buffer[i] = (char)(digit + '0');
    // Prepare for next iteration.
    bignum_times_10(numerator);
  }
  // Generate the last digit.
  uint16_t digit;
  digit = bignum_divide_modulo_int_bignum(numerator, *denominator);
  if (bignum_plus_compare(*numerator, *numerator, *denominator) >= 0) {
    digit++;
  }
  ASSERT(digit <= 10);
  buffer[count - 1] = (char)(digit + '0');
  // Correct bad digits (in case we had a sequence of '9's). Propagate the
  // carry until we hat a non-'9' or til we reach the first digit.
  for (int i = count - 1; i > 0; --i) {
    if (buffer[i] != '0' + 10) break;
    buffer[i] = '0';
    buffer[i - 1]++;
  }
  if (buffer[0] == '0' + 10) {
    // Propagate a carry past the top place.
    buffer[0] = '1';
    (*decimal_point)++;
  }
  *length = count;
}


// Generates 'requested_digits' after the decimal point. It might omit
// trailing '0's. If the input number is too small then no digits at all are
// generated (ex.: 2 fixed digits for 0.00001).
//
// Input verifies:  1 <= (numerator + delta) / denominator < 10.
static void BignumToFixed(int requested_digits, int* decimal_point,
                          bignum* numerator, bignum* denominator,
                          char* buffer, int* length) {
  // Note that we have to look at more than just the requested_digits, since
  // a number could be rounded up. Example: v=0.5 with requested_digits=0.
  // Even though the power of v equals 0 we can't just stop here.
  if (-(*decimal_point) > requested_digits) {
    // The number is definitively too small.
    // Ex: 0.001 with requested_digits == 1.
    // Set decimal-point to -requested_digits. This is what Gay does.
    // Note that it should not have any effect anyways since the string is
    // empty.
    *decimal_point = -requested_digits;
    *length = 0;
    return;
  } else if (-(*decimal_point) == requested_digits) {
    // We only need to verify if the number rounds down or up.
    // Ex: 0.04 and 0.06 with requested_digits == 1.
    ASSERT(*decimal_point == -requested_digits);
    // Initially the fraction lies in range (1, 10]. Multiply the denominator
    // by 10 so that we can compare more easily.
    bignum_times_10(denominator);
    if (bignum_plus_compare(*numerator, *numerator, *denominator) >= 0) {
      // If the fraction is >= 0.5 then we have to include the rounded
      // digit.
      buffer[0] = '1';
      *length = 1;
      (*decimal_point)++;
    } else {
      // Note that we caught most of similar cases earlier.
      *length = 0;
    }
    return;
  } else {
    // The requested digits correspond to the digits after the point.
    // The variable 'needed_digits' includes the digits before the point.
    int needed_digits = (*decimal_point) + requested_digits;
    GenerateCountedDigits(needed_digits, decimal_point,
                          numerator, denominator,
                          buffer, length);
  }
}


// Returns an estimation of k such that 10^(k-1) <= v < 10^k where
// v = f * 2^exponent and 2^52 <= f < 2^53.
// v is hence a normalized double with the given exponent. The output is an
// approximation for the exponent of the decimal approimation .digits * 10^k.
//
// The result might undershoot by 1 in which case 10^k <= v < 10^k+1.
// Note: this property holds for v's upper boundary m+ too.
//    10^k <= m+ < 10^k+1.
//   (see explanation below).
//
// Examples:
//  EstimatePower(0)   => 16
//  EstimatePower(-52) => 0
//
// Note: e >= 0 => EstimatedPower(e) > 0. No similar claim can be made for e<0.
static int EstimatePower(int exponent) {
  // This function estimates log10 of v where v = f*2^e (with e == exponent).
  // Note that 10^floor(log10(v)) <= v, but v <= 10^ceil(log10(v)).
  // Note that f is bounded by its container size. Let p = 53 (the double's
  // significand size). Then 2^(p-1) <= f < 2^p.
  //
  // Given that log10(v) == log2(v)/log2(10) and e+(len(f)-1) is quite close
  // to log2(v) the function is simplified to (e+(len(f)-1)/log2(10)).
  // The computed number undershoots by less than 0.631 (when we compute log3
  // and not log10).
  //
  // Optimization: since we only need an approximated result this computation
  // can be performed on 64 bit integers. On x86/x64 architecture the speedup is
  // not really measurable, though.
  //
  // Since we want to avoid overshooting we decrement by 1e10 so that
  // floating-point imprecisions don't affect us.
  //
  // Explanation for v's boundary m+: the computation takes advantage of
  // the fact that 2^(p-1) <= f < 2^p. Boundaries still satisfy this requirement
  // (even for denormals where the delta can be much more important).

  const double k1Log10 = 0.30102999566398114;  // 1/lg(10)

  // For doubles len(f) == 53 (don't forget the hidden bit).
  const int kSignificandSize = kDoubleSignificandSize;
  double estimate = ceil((exponent + kSignificandSize - 1) * k1Log10 - 1e-10);
  return (int)estimate;
}


// See comments for InitialScaledStartValues.
static void InitialScaledStartValuesPositiveExponent(
    uint64_t significand, int exponent,
    int estimated_power, bool need_boundary_deltas,
    bignum* numerator, bignum* denominator,
    bignum* delta_minus, bignum* delta_plus) {
  // A positive exponent implies a positive power.
  ASSERT(estimated_power >= 0);
  // Since the estimated_power is positive we simply multiply the denominator
  // by 10^estimated_power.

  // numerator = v.
  bignum_assign_uint64(numerator, significand);
  bignum_shift_left(numerator, exponent);
  // denominator = 10^estimated_power.
  bignum_assign_power_uint16(denominator, 10, estimated_power);

  if (need_boundary_deltas) {
    // Introduce a common denominator so that the deltas to the boundaries are
    // integers.
    bignum_shift_left(denominator, 1);
    bignum_shift_left(numerator, 1);
    // Let v = f * 2^e, then m+ - v = 1/2 * 2^e; With the common
    // denominator (of 2) delta_plus equals 2^e.
    bignum_assign_uint16(delta_plus, 1);
    bignum_shift_left(delta_plus, exponent);
    // Same for delta_minus. The adjustments if f == 2^p-1 are done later.
    bignum_assign_uint16(delta_minus, 1);
    bignum_shift_left(delta_minus, exponent);
  }
}


// See comments for InitialScaledStartValues
static void InitialScaledStartValuesNegativeExponentPositivePower(
    uint64_t significand, int exponent,
    int estimated_power, bool need_boundary_deltas,
    bignum* numerator, bignum* denominator,
    bignum* delta_minus, bignum* delta_plus) {
  // v = f * 2^e with e < 0, and with estimated_power >= 0.
  // This means that e is close to 0 (have a look at how estimated_power is
  // computed).

  // numerator = significand
  //  since v = significand * 2^exponent this is equivalent to
  //  numerator = v * / 2^-exponent
  bignum_assign_uint64(numerator, significand);
  // denominator = 10^estimated_power * 2^-exponent (with exponent < 0)
  bignum_assign_power_uint16(denominator, 10, estimated_power);
  bignum_shift_left(denominator, -exponent);

  if (need_boundary_deltas) {
    // Introduce a common denominator so that the deltas to the boundaries are
    // integers.
    bignum_shift_left(denominator, 1);
    bignum_shift_left(numerator, 1);
    // Let v = f * 2^e, then m+ - v = 1/2 * 2^e; With the common
    // denominator (of 2) delta_plus equals 2^e.
    // Given that the denominator already includes v's exponent the distance
    // to the boundaries is simply 1.
    bignum_assign_uint16(delta_plus, 1);
    // Same for delta_minus. The adjustments if f == 2^p-1 are done later.
    bignum_assign_uint16(delta_minus, 1);
  }
}


// See comments for InitialScaledStartValues
static void InitialScaledStartValuesNegativeExponentNegativePower(
    uint64_t significand, int exponent,
    int estimated_power, bool need_boundary_deltas,
    bignum* numerator, bignum* denominator,
    bignum* delta_minus, bignum* delta_plus) {
  // Instead of multiplying the denominator with 10^estimated_power we
  // multiply all values (numerator and deltas) by 10^-estimated_power.

  // Use numerator as temporary container for power_ten.
  bignum* power_ten = numerator;
  bignum_assign_power_uint16(power_ten, 10, -estimated_power);

  if (need_boundary_deltas) {
    // Since power_ten == numerator we must make a copy of 10^estimated_power
    // before we complete the computation of the numerator.
    // delta_plus = delta_minus = 10^estimated_power
    bignum_assign_bignum(delta_plus, *power_ten);
    bignum_assign_bignum(delta_minus, *power_ten);
  }

  // numerator = significand * 2 * 10^-estimated_power
  //  since v = significand * 2^exponent this is equivalent to
  // numerator = v * 10^-estimated_power * 2 * 2^-exponent.
  // Remember: numerator has been abused as power_ten. So no need to assign it
  //  to itself.
  ASSERT(numerator == power_ten);
  bignum_multiply_by_uint64(numerator, significand);

  // denominator = 2 * 2^-exponent with exponent < 0.
  bignum_assign_uint16(denominator, 1);
  bignum_shift_left(denominator, -exponent);

  if (need_boundary_deltas) {
    // Introduce a common denominator so that the deltas to the boundaries are
    // integers.
    bignum_shift_left(numerator, 1);
    bignum_shift_left(denominator, 1);
    // With this shift the boundaries have their correct value, since
    // delta_plus = 10^-estimated_power, and
    // delta_minus = 10^-estimated_power.
    // These assignments have been done earlier.
    // The adjustments if f == 2^p-1 (lower boundary is closer) are done later.
  }
}


// Let v = significand * 2^exponent.
// Computes v / 10^estimated_power exactly, as a ratio of two bignums, numerator
// and denominator. The functions GenerateShortestDigits and
// GenerateCountedDigits will then convert this ratio to its decimal
// representation d, with the required accuracy.
// Then d * 10^estimated_power is the representation of v.
// (Note: the fraction and the estimated_power might get adjusted before
// generating the decimal representation.)
//
// The initial start values consist of:
//  - a scaled numerator: s.t. numerator/denominator == v / 10^estimated_power.
//  - a scaled (common) denominator.
//  optionally (used by GenerateShortestDigits to decide if it has the shortest
//  decimal converting back to v):
//  - v - m-: the distance to the lower boundary.
//  - m+ - v: the distance to the upper boundary.
//
// v, m+, m-, and therefore v - m- and m+ - v all share the same denominator.
//
// Let ep == estimated_power, then the returned values will satisfy:
//  v / 10^ep = numerator / denominator.
//  v's boundarys m- and m+:
//    m- / 10^ep == v / 10^ep - delta_minus / denominator
//    m+ / 10^ep == v / 10^ep + delta_plus / denominator
//  Or in other words:
//    m- == v - delta_minus * 10^ep / denominator;
//    m+ == v + delta_plus * 10^ep / denominator;
//
// Since 10^(k-1) <= v < 10^k    (with k == estimated_power)
//  or       10^k <= v < 10^(k+1)
//  we then have 0.1 <= numerator/denominator < 1
//           or    1 <= numerator/denominator < 10
//
// It is then easy to kickstart the digit-generation routine.
//
// The boundary-deltas are only filled if the mode equals BIGNUM_DTOA_SHORTEST
// or BIGNUM_DTOA_SHORTEST_SINGLE.

static void InitialScaledStartValues(uint64_t significand,
                                     int exponent,
                                     bool lower_boundary_is_closer,
                                     int estimated_power,
                                     bool need_boundary_deltas,
                                     bignum* numerator,
                                     bignum* denominator,
                                     bignum* delta_minus,
                                     bignum* delta_plus) {
  if (exponent >= 0) {
    InitialScaledStartValuesPositiveExponent(
        significand, exponent, estimated_power, need_boundary_deltas,
        numerator, denominator, delta_minus, delta_plus);
  } else if (estimated_power >= 0) {
    InitialScaledStartValuesNegativeExponentPositivePower(
        significand, exponent, estimated_power, need_boundary_deltas,
        numerator, denominator, delta_minus, delta_plus);
  } else {
    InitialScaledStartValuesNegativeExponentNegativePower(
        significand, exponent, estimated_power, need_boundary_deltas,
        numerator, denominator, delta_minus, delta_plus);
  }

  if (need_boundary_deltas && lower_boundary_is_closer) {
    // The lower boundary is closer at half the distance of "normal" numbers.
    // Increase the common denominator and adapt all but the delta_minus.
    bignum_shift_left(denominator, 1);  // *2
    bignum_shift_left(numerator, 1);    // *2
    bignum_shift_left(delta_plus, 1);   // *2
  }
}


// This routine multiplies numerator/denominator so that its values lies in the
// range 1-10. That is after a call to this function we have:
//    1 <= (numerator + delta_plus) /denominator < 10.
// Let numerator the input before modification and numerator' the argument
// after modification, then the output-parameter decimal_point is such that
//  numerator / denominator * 10^estimated_power ==
//    numerator' / denominator' * 10^(decimal_point - 1)
// In some cases estimated_power was too low, and this is already the case. We
// then simply adjust the power so that 10^(k-1) <= v < 10^k (with k ==
// estimated_power) but do not touch the numerator or denominator.
// Otherwise the routine multiplies the numerator and the deltas by 10.
static void FixupMultiply10(int estimated_power, bool is_even,
                            int* decimal_point,
                            bignum* numerator, bignum* denominator,
                            bignum* delta_minus, bignum* delta_plus) {
  bool in_range;
  if (is_even) {
    // For IEEE doubles half-way cases (in decimal system numbers ending with 5)
    // are rounded to the closest floating-point number with even significand.
    in_range = bignum_plus_compare(*numerator, *delta_plus, *denominator) >= 0;
  } else {
    in_range = bignum_plus_compare(*numerator, *delta_plus, *denominator) > 0;
  }
  if (in_range) {
    // Since numerator + delta_plus >= denominator we already have
    // 1 <= numerator/denominator < 10. Simply update the estimated_power.
    *decimal_point = estimated_power + 1;
  } else {
    *decimal_point = estimated_power;
    bignum_times_10(numerator);
    if (bignum_equal(*delta_minus, *delta_plus)) {
      bignum_times_10(delta_minus);
      bignum_assign_bignum(delta_plus, *delta_minus);
    } else {
      bignum_times_10(delta_minus);
      bignum_times_10(delta_plus);
    }
  }
}
