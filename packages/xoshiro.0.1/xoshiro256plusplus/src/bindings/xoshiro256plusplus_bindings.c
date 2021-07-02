#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

static inline uint64_t x256pp_rotl (const uint64_t x, int k) {
	return (x << k) | (x >> (64 - k));
}

uint64_t x256pp_next (uint64_t *s) {
	const uint64_t result = x256pp_rotl(s[0] + s[3], 23) + s[0];

	const uint64_t t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;

	s[3] = x256pp_rotl(s[3], 45);

	return result;
}

void x256pp_jump (uint64_t *s) {
	static const uint64_t JUMP[] = {
    0x180ec6d33cfd0aba,
    0xd5a61266f0c9392c,
    0xa9582618e03fc9aa,
    0x39abdc4529b1661c
  };

	uint64_t s0 = 0;
	uint64_t s1 = 0;
	uint64_t s2 = 0;
	uint64_t s3 = 0;

	for(int i = 0; i < (sizeof(JUMP) / sizeof(*JUMP)); i++)
		for(int b = 0; b < 64; b++) {
			if (JUMP[i] & UINT64_C(1) << b) {
				s0 ^= s[0];
				s1 ^= s[1];
				s2 ^= s[2];
				s3 ^= s[3];
			}
			x256pp_next(s);
		}

	s[0] = s0;
	s[1] = s1;
	s[2] = s2;
	s[3] = s3;
}

void x256pp_long_jump (uint64_t *s) {
	static const uint64_t LONG_JUMP[] = {
    0x76e15d3efefdcbbf,
    0xc5004e441c522fb3,
    0x77710069854ee241,
    0x39109bb02acbe635
  };

	uint64_t s0 = 0;
	uint64_t s1 = 0;
	uint64_t s2 = 0;
	uint64_t s3 = 0;

	for(int i = 0; i < (sizeof(LONG_JUMP) / sizeof(*LONG_JUMP)); i++)
		for(int b = 0; b < 64; b++) {
			if (LONG_JUMP[i] & UINT64_C(1) << b) {
				s0 ^= s[0];
				s1 ^= s[1];
				s2 ^= s[2];
				s3 ^= s[3];
			}
			x256pp_next(s);
		}

	s[0] = s0;
	s[1] = s1;
	s[2] = s2;
	s[3] = s3;
}
