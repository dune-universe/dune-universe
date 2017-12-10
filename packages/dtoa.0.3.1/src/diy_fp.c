/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>

#include "diy_fp.h"
#include "utils.h"

const int kDiyFpSignificandSize = 64;
static const uint64_t kUint64MSB = UINT64_2PART_C(0x80000000, 00000000);

diy_fp diy_fp_minus(diy_fp a, diy_fp b) {
  diy_fp r;
  assert(a.e == b.e);
  assert(a.f >= b.f);
  r.f = a.f - b.f;
  r.e = a.e;
  return r;
}

diy_fp diy_fp_multiply(diy_fp x, diy_fp y) {
  // Simply "emulates" a 128 bit multiplication.
  // However: the resulting number only contains 64 bits. The least
  // significant 64 bits are only used for rounding the most significant 64
  // bits.
  diy_fp r;
  const uint64_t kM32 = 0xFFFFFFFFU;
  uint64_t a = x.f >> 32;
  uint64_t b = x.f & kM32;
  uint64_t c = y.f >> 32;
  uint64_t d = y.f & kM32;
  uint64_t ac = a * c;
  uint64_t bc = b * c;
  uint64_t ad = a * d;
  uint64_t bd = b * d;
  uint64_t tmp = (bd >> 32) + (ad & kM32) + (bc & kM32);
  // By adding 1U << 31 to tmp we round the final result.
  // Halfway cases will be round up.
  tmp += 1U << 31;
  r.f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32);
  r.e = x.e + y.e + 64;
  return r;
}

diy_fp diy_fp_normalize(diy_fp x) {
  diy_fp r;
  assert(x.f != 0);
  uint64_t significand = x.f;
  int exponent = x.e;

  // This method is mainly called for normalizing boundaries. In general
  // boundaries need to be shifted by 10 bits. We thus optimize for this case.
  const uint64_t k10MSBits = UINT64_2PART_C(0xFFC00000, 00000000);
  while ((significand & k10MSBits) == 0) {
    significand <<= 10;
    exponent -= 10;
  }
  while ((significand & kUint64MSB) == 0) {
    significand <<= 1;
    exponent--;
  }
  r.f = significand;
  r.e = exponent;
  return r;
}
