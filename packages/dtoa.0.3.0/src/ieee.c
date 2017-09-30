/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>
#include <stdint.h>

#include "diy_fp.h"
#include "ieee.h"
#include "utils.h"

const uint64_t kSignMask = UINT64_2PART_C(0x80000000, 00000000);
const uint64_t kExponentMask = UINT64_2PART_C(0x7FF00000, 00000000);
const uint64_t kSignificandMask = UINT64_2PART_C(0x000FFFFF, FFFFFFFF);
const uint64_t kDoubleHiddenBit = UINT64_2PART_C(0x00100000, 00000000);
const int kDoublePhysicalSignificandSize = 52;  // Excludes the hidden bit.
const int kDoubleSignificandSize = 53;

const int kDoubleExponentBias = 0x433; // 0x3FF + kDoublePhysicalSignificandSize
const int kDoubleDenormalExponent = -0x432; // -kDoubleExponentBias + 1;

static uint64_t as_uint64(double d) {
  union { double d; uint64_t u64; } u;
  u.d = d;
  return u.u64;
}

static bool d64_is_denormal(uint64_t d64) {
  return (d64 & kExponentMask) == 0;
}

// Returns true if the double is a denormal.
bool double_is_denormal(double d) {
  return d64_is_denormal(as_uint64(d));
}

static bool d64_is_special(uint64_t d64) {
  return (d64 & kExponentMask) == kExponentMask;
}

bool double_is_special(double d) {
  return d64_is_special(as_uint64(d));
}

static uint64_t d64_significand(uint64_t d64) {
  uint64_t significand = d64 & kSignificandMask;
  if (!d64_is_denormal(d64)) {
    return significand + kDoubleHiddenBit;
  } else {
    return significand;
  }
}

uint64_t double_significand(double d) {
  return d64_significand(as_uint64(d));
}

static int d64_exponent(uint64_t d64) {
  if (d64_is_denormal(d64)) return kDoubleDenormalExponent;

  int biased_e = (int)((d64 & kExponentMask) >> kDoublePhysicalSignificandSize);
  return biased_e - kDoubleExponentBias;
}

int double_exponent(double d) {
  return d64_exponent(as_uint64(d));
}

static int d64_sign(uint64_t d64) {
  return (d64 & kSignMask) == 0? 1: -1;
}

int double_sign(double d) {
  return d64_sign(as_uint64(d));
}

// The value encoded by this Double must be greater or equal to +0.0.
// It must not be special (infinity, or NaN).
diy_fp double_as_diy_fp(double d) {
  diy_fp r;
  uint64_t d64 = as_uint64(d);
  ASSERT(d64_sign(d64) > 0);
  ASSERT(!d64_is_special(d64));
  r.f = d64_significand(d64);
  r.e = d64_exponent(d64);
  return r;
}

diy_fp double_as_normalized_diy_fp(double d) {
  diy_fp r;
  ASSERT(d > 0.0);
  uint64_t d64 = as_uint64(d);
  uint64_t f = d64_significand(d64);
  int e = d64_exponent(d64);

  // The current double could be a denormal.
  while ((f & kDoubleHiddenBit) == 0) {
    f <<= 1;
    e--;
  }
  // Do the final shifts in one go.
  f <<= kDiyFpSignificandSize - kDoubleSignificandSize;
  e -= kDiyFpSignificandSize - kDoubleSignificandSize;
  r.f = f;
  r.e = e;
  return r;
}

bool double_lower_boundary_is_closer(double d) {
  // The boundary is closer if the significand is of the form f == 2^p-1 then
  // the lower boundary is closer.
  // Think of v = 1000e10 and v- = 9999e9.
  // Then the boundary (== (v - v-)/2) is not just at a distance of 1e9 but
  // at a distance of 1e8.
  // The only exception is for the smallest normal: the largest denormal is
  // at the same distance as its successor.
  // Note: denormals have the same exponent as the smallest normals.
  uint64_t d64 = as_uint64(d);
  bool physical_significand_is_zero = ((d64 & kSignificandMask) == 0);
  return physical_significand_is_zero &&
    (d64_exponent(d64) != kDoubleDenormalExponent);
}

void double_normalized_boundaries(
  double d,
  diy_fp* out_m_minus,
  diy_fp* out_m_plus
) {
  ASSERT(d > 0.0);
  diy_fp v = double_as_diy_fp(d);
  diy_fp tmp = { (v.f << 1) + 1, v.e - 1 };
  diy_fp m_plus = diy_fp_normalize(tmp);
  diy_fp m_minus;
  if (double_lower_boundary_is_closer(d)) {
    m_minus.f = (v.f << 2) - 1;
    m_minus.e = v.e - 2;
  } else {
    m_minus.f = (v.f << 1) - 1;
    m_minus.e = v.e - 1;
  }
  m_minus.f = m_minus.f << (m_minus.e - m_plus.e);
  m_minus.e = m_plus.e;
  *out_m_plus = m_plus;
  *out_m_minus = m_minus;
}
