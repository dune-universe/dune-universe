/* MIT License
 *
 * Copyright (c) 2016-2020 INRIA, CMU and Microsoft Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#include "Hacl_Hash.h"

void Hacl_Hash_SHA2_update_multi_512(uint64_t *s, uint8_t *blocks, uint32_t n_blocks)
{
  for (uint32_t i = (uint32_t)0U; i < n_blocks; i++)
  {
    uint32_t sz = (uint32_t)128U;
    uint8_t *block = blocks + sz * i;
    Hacl_Hash_Core_SHA2_update_512(s, block);
  }
}

void
Hacl_Hash_SHA2_update_last_512(
  uint64_t *s,
  FStar_UInt128_uint128 prev_len,
  uint8_t *input,
  uint32_t input_len
)
{
  uint32_t blocks_n = input_len / (uint32_t)128U;
  uint32_t blocks_len = blocks_n * (uint32_t)128U;
  uint8_t *blocks = input;
  uint32_t rest_len = input_len - blocks_len;
  uint8_t *rest = input + blocks_len;
  Hacl_Hash_SHA2_update_multi_512(s, blocks, blocks_n);
  FStar_UInt128_uint128
  total_input_len =
    FStar_UInt128_add(prev_len,
      FStar_UInt128_uint64_to_uint128((uint64_t)input_len));
  uint32_t
  pad_len =
    (uint32_t)1U
    +
      ((uint32_t)256U
      -
        ((uint32_t)17U
        + (uint32_t)(FStar_UInt128_uint128_to_uint64(total_input_len) % (uint64_t)(uint32_t)128U)))
      % (uint32_t)128U
    + (uint32_t)16U;
  uint32_t tmp_len = rest_len + pad_len;
  uint8_t tmp_twoblocks[256U] = { 0U };
  uint8_t *tmp = tmp_twoblocks;
  uint8_t *tmp_rest = tmp;
  uint8_t *tmp_pad = tmp + rest_len;
  memcpy(tmp_rest, rest, rest_len * sizeof (rest[0U]));
  Hacl_Hash_Core_SHA2_pad_512(total_input_len, tmp_pad);
  Hacl_Hash_SHA2_update_multi_512(s, tmp, tmp_len / (uint32_t)128U);
}

void Hacl_Hash_SHA2_hash_512(uint8_t *input, uint32_t input_len, uint8_t *dst)
{
  uint64_t
  s[8U] =
    {
      (uint64_t)0x6a09e667f3bcc908U, (uint64_t)0xbb67ae8584caa73bU, (uint64_t)0x3c6ef372fe94f82bU,
      (uint64_t)0xa54ff53a5f1d36f1U, (uint64_t)0x510e527fade682d1U, (uint64_t)0x9b05688c2b3e6c1fU,
      (uint64_t)0x1f83d9abfb41bd6bU, (uint64_t)0x5be0cd19137e2179U
    };
  uint32_t blocks_n = input_len / (uint32_t)128U;
  uint32_t blocks_len = blocks_n * (uint32_t)128U;
  uint8_t *blocks = input;
  uint32_t rest_len = input_len - blocks_len;
  uint8_t *rest = input + blocks_len;
  Hacl_Hash_SHA2_update_multi_512(s, blocks, blocks_n);
  Hacl_Hash_SHA2_update_last_512(s,
    FStar_UInt128_uint64_to_uint128((uint64_t)blocks_len),
    rest,
    rest_len);
  Hacl_Hash_Core_SHA2_finish_512(s, dst);
}

static uint64_t
k384_512[80U] =
  {
    (uint64_t)0x428a2f98d728ae22U, (uint64_t)0x7137449123ef65cdU, (uint64_t)0xb5c0fbcfec4d3b2fU,
    (uint64_t)0xe9b5dba58189dbbcU, (uint64_t)0x3956c25bf348b538U, (uint64_t)0x59f111f1b605d019U,
    (uint64_t)0x923f82a4af194f9bU, (uint64_t)0xab1c5ed5da6d8118U, (uint64_t)0xd807aa98a3030242U,
    (uint64_t)0x12835b0145706fbeU, (uint64_t)0x243185be4ee4b28cU, (uint64_t)0x550c7dc3d5ffb4e2U,
    (uint64_t)0x72be5d74f27b896fU, (uint64_t)0x80deb1fe3b1696b1U, (uint64_t)0x9bdc06a725c71235U,
    (uint64_t)0xc19bf174cf692694U, (uint64_t)0xe49b69c19ef14ad2U, (uint64_t)0xefbe4786384f25e3U,
    (uint64_t)0x0fc19dc68b8cd5b5U, (uint64_t)0x240ca1cc77ac9c65U, (uint64_t)0x2de92c6f592b0275U,
    (uint64_t)0x4a7484aa6ea6e483U, (uint64_t)0x5cb0a9dcbd41fbd4U, (uint64_t)0x76f988da831153b5U,
    (uint64_t)0x983e5152ee66dfabU, (uint64_t)0xa831c66d2db43210U, (uint64_t)0xb00327c898fb213fU,
    (uint64_t)0xbf597fc7beef0ee4U, (uint64_t)0xc6e00bf33da88fc2U, (uint64_t)0xd5a79147930aa725U,
    (uint64_t)0x06ca6351e003826fU, (uint64_t)0x142929670a0e6e70U, (uint64_t)0x27b70a8546d22ffcU,
    (uint64_t)0x2e1b21385c26c926U, (uint64_t)0x4d2c6dfc5ac42aedU, (uint64_t)0x53380d139d95b3dfU,
    (uint64_t)0x650a73548baf63deU, (uint64_t)0x766a0abb3c77b2a8U, (uint64_t)0x81c2c92e47edaee6U,
    (uint64_t)0x92722c851482353bU, (uint64_t)0xa2bfe8a14cf10364U, (uint64_t)0xa81a664bbc423001U,
    (uint64_t)0xc24b8b70d0f89791U, (uint64_t)0xc76c51a30654be30U, (uint64_t)0xd192e819d6ef5218U,
    (uint64_t)0xd69906245565a910U, (uint64_t)0xf40e35855771202aU, (uint64_t)0x106aa07032bbd1b8U,
    (uint64_t)0x19a4c116b8d2d0c8U, (uint64_t)0x1e376c085141ab53U, (uint64_t)0x2748774cdf8eeb99U,
    (uint64_t)0x34b0bcb5e19b48a8U, (uint64_t)0x391c0cb3c5c95a63U, (uint64_t)0x4ed8aa4ae3418acbU,
    (uint64_t)0x5b9cca4f7763e373U, (uint64_t)0x682e6ff3d6b2b8a3U, (uint64_t)0x748f82ee5defb2fcU,
    (uint64_t)0x78a5636f43172f60U, (uint64_t)0x84c87814a1f0ab72U, (uint64_t)0x8cc702081a6439ecU,
    (uint64_t)0x90befffa23631e28U, (uint64_t)0xa4506cebde82bde9U, (uint64_t)0xbef9a3f7b2c67915U,
    (uint64_t)0xc67178f2e372532bU, (uint64_t)0xca273eceea26619cU, (uint64_t)0xd186b8c721c0c207U,
    (uint64_t)0xeada7dd6cde0eb1eU, (uint64_t)0xf57d4f7fee6ed178U, (uint64_t)0x06f067aa72176fbaU,
    (uint64_t)0x0a637dc5a2c898a6U, (uint64_t)0x113f9804bef90daeU, (uint64_t)0x1b710b35131c471bU,
    (uint64_t)0x28db77f523047d84U, (uint64_t)0x32caab7b40c72493U, (uint64_t)0x3c9ebe0a15c9bebcU,
    (uint64_t)0x431d67c49c100d4cU, (uint64_t)0x4cc5d4becb3e42b6U, (uint64_t)0x597f299cfc657e2aU,
    (uint64_t)0x5fcb6fab3ad6faecU, (uint64_t)0x6c44198c4a475817U
  };

void Hacl_Hash_Core_SHA2_update_512(uint64_t *hash, uint8_t *block)
{
  uint64_t hash1[8U] = { 0U };
  uint64_t computed_ws[80U] = { 0U };
  for (uint32_t i = (uint32_t)0U; i < (uint32_t)80U; i++)
  {
    if (i < (uint32_t)16U)
    {
      uint8_t *b = block + i * (uint32_t)8U;
      uint64_t u = load64_be(b);
      computed_ws[i] = u;
    }
    else
    {
      uint64_t t16 = computed_ws[i - (uint32_t)16U];
      uint64_t t15 = computed_ws[i - (uint32_t)15U];
      uint64_t t7 = computed_ws[i - (uint32_t)7U];
      uint64_t t2 = computed_ws[i - (uint32_t)2U];
      uint64_t
      s1 =
        (t2 >> (uint32_t)19U | t2 << (uint32_t)45U)
        ^ ((t2 >> (uint32_t)61U | t2 << (uint32_t)3U) ^ t2 >> (uint32_t)6U);
      uint64_t
      s0 =
        (t15 >> (uint32_t)1U | t15 << (uint32_t)63U)
        ^ ((t15 >> (uint32_t)8U | t15 << (uint32_t)56U) ^ t15 >> (uint32_t)7U);
      uint64_t w = s1 + t7 + s0 + t16;
      computed_ws[i] = w;
    }
  }
  memcpy(hash1, hash, (uint32_t)8U * sizeof (hash[0U]));
  for (uint32_t i = (uint32_t)0U; i < (uint32_t)80U; i++)
  {
    uint64_t a0 = hash1[0U];
    uint64_t b0 = hash1[1U];
    uint64_t c0 = hash1[2U];
    uint64_t d0 = hash1[3U];
    uint64_t e0 = hash1[4U];
    uint64_t f0 = hash1[5U];
    uint64_t g0 = hash1[6U];
    uint64_t h02 = hash1[7U];
    uint64_t w = computed_ws[i];
    uint64_t
    t1 =
      h02
      +
        ((e0 >> (uint32_t)14U | e0 << (uint32_t)50U)
        ^
          ((e0 >> (uint32_t)18U | e0 << (uint32_t)46U)
          ^ (e0 >> (uint32_t)41U | e0 << (uint32_t)23U)))
      + ((e0 & f0) ^ (~e0 & g0))
      + k384_512[i]
      + w;
    uint64_t
    t2 =
      ((a0 >> (uint32_t)28U | a0 << (uint32_t)36U)
      ^ ((a0 >> (uint32_t)34U | a0 << (uint32_t)30U) ^ (a0 >> (uint32_t)39U | a0 << (uint32_t)25U)))
      + ((a0 & b0) ^ ((a0 & c0) ^ (b0 & c0)));
    hash1[0U] = t1 + t2;
    hash1[1U] = a0;
    hash1[2U] = b0;
    hash1[3U] = c0;
    hash1[4U] = d0 + t1;
    hash1[5U] = e0;
    hash1[6U] = f0;
    hash1[7U] = g0;
  }
  for (uint32_t i = (uint32_t)0U; i < (uint32_t)8U; i++)
  {
    uint64_t xi = hash[i];
    uint64_t yi = hash1[i];
    hash[i] = xi + yi;
  }
}

void Hacl_Hash_Core_SHA2_pad_512(FStar_UInt128_uint128 len, uint8_t *dst)
{
  uint8_t *dst1 = dst;
  dst1[0U] = (uint8_t)0x80U;
  uint8_t *dst2 = dst + (uint32_t)1U;
  uint32_t
  len_zero =
    ((uint32_t)256U
    - ((uint32_t)17U + (uint32_t)(FStar_UInt128_uint128_to_uint64(len) % (uint64_t)(uint32_t)128U)))
    % (uint32_t)128U;
  for
  (uint32_t
    i = (uint32_t)0U;
    i
    <
      ((uint32_t)256U
      -
        ((uint32_t)17U
        + (uint32_t)(FStar_UInt128_uint128_to_uint64(len) % (uint64_t)(uint32_t)128U)))
      % (uint32_t)128U;
    i++)
  {
    dst2[i] = (uint8_t)0U;
  }
  uint8_t
  *dst3 =
    dst
    +
      (uint32_t)1U
      +
        ((uint32_t)256U
        -
          ((uint32_t)17U
          + (uint32_t)(FStar_UInt128_uint128_to_uint64(len) % (uint64_t)(uint32_t)128U)))
        % (uint32_t)128U;
  FStar_UInt128_uint128 len_ = FStar_UInt128_shift_left(len, (uint32_t)3U);
  store128_be(dst3, len_);
}

void Hacl_Hash_Core_SHA2_finish_512(uint64_t *s, uint8_t *dst)
{
  uint64_t *uu____0 = s;
  for (uint32_t i = (uint32_t)0U; i < (uint32_t)8U; i++)
  {
    store64_be(dst + i * (uint32_t)8U, uu____0[i]);
  }
}

