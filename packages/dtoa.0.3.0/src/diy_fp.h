/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * Copyright (c) 2010, the V8 project authors.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef OCAML_DTOA_DIY_FP_H_
#define OCAML_DTOA_DIY_FP_H_

#include <stdint.h>

typedef struct diy_fp {
  uint64_t f;
  int e;
} diy_fp;

extern const int kDiyFpSignificandSize;

// Returns a - b.
// The exponents of both numbers must be the same and this must be bigger
// than other. The result will not be normalized.
diy_fp diy_fp_minus(diy_fp a, diy_fp b);

// returns a * b;
diy_fp diy_fp_multiply(diy_fp a, diy_fp b);

diy_fp diy_fp_normalize(diy_fp x);

#endif  // OCAML_DTOA_DIY_FP_H_
