/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "bignum_dtoa.h"
#include "fast_dtoa.h"

typedef enum {
  NO_FLAGS = 0,
  LEADING_ZERO = 1, // 0.123 instead of .123
  PLUS_IN_EXPONENT = 2, // 1e+3 instead of 1e3
  PAD_EXPONENT = 4, // 1e+03 instead of 1e+3
} flags;

static int handle_special(double v, char *dst) {
  int fp_class = fpclassify(v);
  if (fp_class == FP_NAN) {
    return snprintf(dst, 4, "NaN");
  }
  if (fp_class == FP_ZERO) {
    return snprintf(dst, 2, "0");
  }
  if (fp_class == FP_INFINITE) {
    if (v < 0) {
      return snprintf(dst, 10, "-Infinity");
    } else {
      return snprintf(dst, 9, "Infinity");
    }
  }
  return -1; // not special
}

static int i_to_str(int val, char *str) {
  int len, i;
  char *s;
  char *begin = str;
  if (val < 0) { *str++ = '-'; val = -val; }
  s = str;

  for(;;)
  {
    int ni = val / 10;
    int digit = val - ni*10;
    *s++ = (char)('0' + digit);
    if (ni == 0)
      break;
    val = ni;
  }
  *s = '\0';
  len = (int)(s - str);
  for(i = 0; i < len/2; ++i)
  {
    char ch = str[i];
    str[i] = str[len-1-i];
    str[len-1-i] = ch;
  }

  return (int)(s - begin);
}

static int decimal(char *dst, int len, int decimal_point, flags flgs) {
  int leading_zero = flgs & LEADING_ZERO;
  int d_exp = decimal_point - len, written = 0, shift = 0, j;
  if (decimal_point <= 0) {
    if (leading_zero) shift++;
    shift++; // decimal point
    // shift digits over, then fill in zeros and decimal point
    memmove(dst + shift - decimal_point, dst, len);
    if (leading_zero) dst[0] = '0';
    dst[leading_zero ? 1 : 0] = '.';
    for (j = 0; j < -decimal_point; ++j) {
      dst[shift + j] = '0';
    }
    written += shift + j;
  } else if (decimal_point > len) {
    // right pad with zeros
    while (d_exp-- > 0) {
      dst[len + written++] = '0';
    }
  } else if (len > 1 && d_exp < 0) {
    // stick decimal in the middle
    memmove(dst + decimal_point + 1, dst + decimal_point, -d_exp);
    dst[decimal_point] = '.';
    written++;
  }
  return written;
}

static int scientific(char *dst, int len, int decimal_point, flags flgs) {
  int written = 0;
  int exponent = decimal_point - 1;

  // if only 1 digit, don't need a decimal, e.g. 0.0000001 -> 1e-7 or
  // 100000000000000000000.0 -> 1e21
  if (len > 1) {
    memmove(dst + 1, dst, len);
    dst[1] = '.';
    written++;
  }
  dst[len + written++] = 'e';

  if (flgs & PLUS_IN_EXPONENT && exponent > 0) {
    dst[len + written++] = '+';
  }
  if (flgs & PAD_EXPONENT && exponent > 0 && exponent < 10) {
    dst[len + written++] = '0';
  }

  written += i_to_str(exponent, dst+len+written);

  return written;
}

static int exponential(char *dst, int len, int d_exp) {
  int written = 0;
  dst[len + written++] = 'e';
  written += i_to_str(d_exp, dst+len+written);
  return written;
}

static int shortest_dtoa(double v, char *dst, int low_exp, int high_exp) {
  int d_exp, len, success, i, decimal_point, exp_len;
  char *s2 = dst;
  assert(dst);

  i = handle_special(v, dst);
  if (i >= 0) return i;

  // Grisu3 doesn't handle negative values, so emit the - and negate `v`.
  if (v < 0) {
    *s2++ = '-';
    v = -v;
  }

  success = fast_dtoa(v, FAST_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  // If the fast dtoa didn't succeed use the slower bignum version.
  if (!success) {
    bignum_dtoa(v, BIGNUM_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  }

  // We now have an integer string of form "151324135" in s2 and a base-10
  // exponent for that number in d_exp.

  d_exp = decimal_point - len;

  // calculate length of printing the exponent.
  // cheaper than `(int)log10(d_exp) + 1` (?) and handles negatives
  exp_len =
    d_exp < -99 ? 4 :
    d_exp < -9 ? 3 :
    d_exp < 0 ? 2 :
    d_exp < 10 ? 1 :
    d_exp < 100 ? 2 :
    3;

  if (decimal_point < 0) {
    // decimal length = len + 1 (for .) + abs(len+d_exp) (for 0's)
    // exponential length = len + 1 (for e) + exp_len
    if (-decimal_point <= exp_len) {
      len += decimal(s2, len, decimal_point, NO_FLAGS);
    } else {
      len += exponential(s2, len, d_exp);
    }
  } else {
    // decimal length = len + d_exp (for 0's)
    // exponential length = len + 1 (for e) + exp_len
    if (d_exp <= 1 + exp_len) {
      len += decimal(s2, len, decimal_point, NO_FLAGS);
    } else {
      len += exponential(s2, len, d_exp);
    }
  }

  s2[len] = '\0';
  return (int)(s2+len-dst);
}

static int ecma_dtoa(double v, char *dst) {
  int decimal_point, len, success, i, exponent;
  char *s2 = dst;
  assert(dst);

  i = handle_special(v, dst);
  if (i >= 0) return i;

  // Grisu3 doesn't handle negative values, so emit the - and negate `v`.
  if (v < 0) {
    *s2++ = '-';
    v = -v;
  }

  success = fast_dtoa(v, FAST_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  // If the fast dtoa didn't succeed use the slower bignum version.
  if (!success) {
    bignum_dtoa(v, BIGNUM_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  }

  // We now have an integer string of form "151324135" in s2 and the offset of
  // the decimal point.
  //
  // if printed in scientific notation, what would the exponent be? e.g.
  // 0.999999 is s2 == "999999", len == 6, decimal_point == 0, so in scientific
  // notation it would be 9.99999e-1.
  //
  // note: we don't actually print 0.999999 in scientific notation given the
  // decimal_in_shortest_low rule mentioned above. this is just an example.
  exponent = decimal_point - 1;

  if (-6 <= exponent && exponent < 21) {
    len += decimal(s2, len, decimal_point, LEADING_ZERO);
  } else {
    len += scientific(s2, len, decimal_point, PLUS_IN_EXPONENT);
  }

  s2[len] = '\0';
  return (int)(s2+len-dst);
}

// Like David M. Gay's g_fmt, but using grisu3.
// http://www.netlib.org/fp/g_fmt.c
static int grisu3_g_fmt(double v, char *dst) {
  int len, success, i, decimal_point;
  char *s2 = dst;
  assert(dst);

  i = handle_special(v, dst);
  if (i >= 0) return i;

  // Grisu3 doesn't handle negative values, so emit the - and negate `v`.
  if (v < 0) {
    *s2++ = '-';
    v = -v;
  }

  success = fast_dtoa(v, FAST_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  // If the fast dtoa didn't succeed use the slower bignum version.
  if (!success) {
    bignum_dtoa(v, BIGNUM_DTOA_SHORTEST, 0, s2, &len, &decimal_point);
  }

  if (-3 <= decimal_point && decimal_point < len + 6) {
    len += decimal(s2, len, decimal_point, NO_FLAGS);
  } else {
    len += scientific(s2, len, decimal_point, PLUS_IN_EXPONENT | PAD_EXPONENT);
  }

  s2[len] = '\0';
  return (int)(s2+len-dst);
}

CAMLprim value flow_shortest_string_of_float(value num) {
  CAMLparam1(num);
  char str[32]; // the max length is probably 18, but better safe than sorry
  int len = shortest_dtoa(Double_val(num), str, -3, 3);
  assert(len > 0 && len < 25);
  CAMLreturn(caml_copy_string(str));
}

CAMLprim value flow_ecma_string_of_float(value num) {
  CAMLparam1(num);
  char str[32]; // the max length is probably 18, but better safe than sorry
  int len = ecma_dtoa(Double_val(num), str);
  assert(len > 0 && len < 25);
  CAMLreturn(caml_copy_string(str));
}

CAMLprim value flow_g_fmt(value num) {
  CAMLparam1(num);
  char str[32]; // the max length is probably 18, but better safe than sorry
  int len = grisu3_g_fmt(Double_val(num), str);
  assert(len > 0 && len < 25);
  CAMLreturn(caml_copy_string(str));
}
