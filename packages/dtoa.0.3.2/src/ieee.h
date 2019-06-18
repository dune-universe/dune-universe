/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef OCAML_DTOA_IEEE_H_
#define OCAML_DTOA_IEEE_H_

#include <stdbool.h>
#include "diy_fp.h"
#include "utils.h"

extern const uint64_t kSignMask;
extern const uint64_t kExponentMask;
extern const uint64_t kSignificandMask;
extern const uint64_t kDoubleHiddenBit;
extern const int kPhysicalSignificandSize;
extern const int kDoubleSignificandSize;

// Returns true if the double is a denormal.
bool double_is_denormal(double d);

// We consider denormals not to be special.
// Hence only Infinity and NaN are special.
bool double_is_special(double d);

// The value encoded by this double must be strictly greater than 0.
diy_fp double_as_normalized_diy_fp(double d);

// Computes the two boundaries of this.
// The bigger boundary (m_plus) is normalized. The lower boundary has the same
// exponent as m_plus.
// Precondition: the value encoded by this Single must be greater than 0.
void double_normalized_boundaries(
  double d, diy_fp* out_m_minus, diy_fp* out_m_plus
);

int double_exponent(double d);
uint64_t double_significand(double d);
bool double_lower_boundary_is_closer(double d);

#endif  // OCAML_DTOA_IEEE_H_
