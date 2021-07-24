
#include "Hacl_Bignum256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Bignum256_add(value x3, value x2, value x1)
{
   uint64_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   uint64_t* x5 = CTYPES_ADDR_OF_FATPTR(x2);
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x1);
   uint64_t x7 = Hacl_Bignum256_add(x4, x5, x6);
   return integers_copy_uint64(x7);
}
value _2_Hacl_Bignum256_sub(value x10, value x9, value x8)
{
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   uint64_t* x12 = CTYPES_ADDR_OF_FATPTR(x9);
   uint64_t* x13 = CTYPES_ADDR_OF_FATPTR(x8);
   uint64_t x14 = Hacl_Bignum256_sub(x11, x12, x13);
   return integers_copy_uint64(x14);
}
value _3_Hacl_Bignum256_mul(value x17, value x16, value x15)
{
   uint64_t* x18 = CTYPES_ADDR_OF_FATPTR(x17);
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t* x20 = CTYPES_ADDR_OF_FATPTR(x15);
   Hacl_Bignum256_mul(x18, x19, x20);
   return Val_unit;
}
value _4_Hacl_Bignum256_sqr(value x23, value x22)
{
   uint64_t* x24 = CTYPES_ADDR_OF_FATPTR(x23);
   uint64_t* x25 = CTYPES_ADDR_OF_FATPTR(x22);
   Hacl_Bignum256_sqr(x24, x25);
   return Val_unit;
}
value _5_Hacl_Bignum256_mod(value x29, value x28, value x27)
{
   uint64_t* x30 = CTYPES_ADDR_OF_FATPTR(x29);
   uint64_t* x31 = CTYPES_ADDR_OF_FATPTR(x28);
   uint64_t* x32 = CTYPES_ADDR_OF_FATPTR(x27);
   _Bool x33 = Hacl_Bignum256_mod(x30, x31, x32);
   return Val_bool(x33);
}
value _6_Hacl_Bignum256_mod_exp_vartime(value x38, value x37, value x36,
                                        value x35, value x34)
{
   uint64_t* x39 = CTYPES_ADDR_OF_FATPTR(x38);
   uint64_t* x40 = CTYPES_ADDR_OF_FATPTR(x37);
   uint32_t x41 = Uint32_val(x36);
   uint64_t* x44 = CTYPES_ADDR_OF_FATPTR(x35);
   uint64_t* x45 = CTYPES_ADDR_OF_FATPTR(x34);
   _Bool x46 = Hacl_Bignum256_mod_exp_vartime(x39, x40, x41, x44, x45);
   return Val_bool(x46);
}
value _7_Hacl_Bignum256_mod_exp_consttime(value x51, value x50, value x49,
                                          value x48, value x47)
{
   uint64_t* x52 = CTYPES_ADDR_OF_FATPTR(x51);
   uint64_t* x53 = CTYPES_ADDR_OF_FATPTR(x50);
   uint32_t x54 = Uint32_val(x49);
   uint64_t* x57 = CTYPES_ADDR_OF_FATPTR(x48);
   uint64_t* x58 = CTYPES_ADDR_OF_FATPTR(x47);
   _Bool x59 = Hacl_Bignum256_mod_exp_consttime(x52, x53, x54, x57, x58);
   return Val_bool(x59);
}
value _8_Hacl_Bignum256_mod_inv_prime_vartime(value x62, value x61,
                                              value x60)
{
   uint64_t* x63 = CTYPES_ADDR_OF_FATPTR(x62);
   uint64_t* x64 = CTYPES_ADDR_OF_FATPTR(x61);
   uint64_t* x65 = CTYPES_ADDR_OF_FATPTR(x60);
   _Bool x66 = Hacl_Bignum256_mod_inv_prime_vartime(x63, x64, x65);
   return Val_bool(x66);
}
value _9_Hacl_Bignum256_mont_ctx_init(value x67)
{
   uint64_t* x68 = CTYPES_ADDR_OF_FATPTR(x67);
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x69 =
   Hacl_Bignum256_mont_ctx_init(x68);
   return CTYPES_FROM_PTR(x69);
}
value _10_Hacl_Bignum256_mont_ctx_free(value x70)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x71 =
   CTYPES_ADDR_OF_FATPTR(x70);
   Hacl_Bignum256_mont_ctx_free(x71);
   return Val_unit;
}
value _11_Hacl_Bignum256_mod_precomp(value x75, value x74, value x73)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x76 =
   CTYPES_ADDR_OF_FATPTR(x75);
   uint64_t* x77 = CTYPES_ADDR_OF_FATPTR(x74);
   uint64_t* x78 = CTYPES_ADDR_OF_FATPTR(x73);
   Hacl_Bignum256_mod_precomp(x76, x77, x78);
   return Val_unit;
}
value _12_Hacl_Bignum256_mod_exp_vartime_precomp(value x84, value x83,
                                                 value x82, value x81,
                                                 value x80)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x85 =
   CTYPES_ADDR_OF_FATPTR(x84);
   uint64_t* x86 = CTYPES_ADDR_OF_FATPTR(x83);
   uint32_t x87 = Uint32_val(x82);
   uint64_t* x90 = CTYPES_ADDR_OF_FATPTR(x81);
   uint64_t* x91 = CTYPES_ADDR_OF_FATPTR(x80);
   Hacl_Bignum256_mod_exp_vartime_precomp(x85, x86, x87, x90, x91);
   return Val_unit;
}
value _13_Hacl_Bignum256_mod_exp_consttime_precomp(value x97, value x96,
                                                   value x95, value x94,
                                                   value x93)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x98 =
   CTYPES_ADDR_OF_FATPTR(x97);
   uint64_t* x99 = CTYPES_ADDR_OF_FATPTR(x96);
   uint32_t x100 = Uint32_val(x95);
   uint64_t* x103 = CTYPES_ADDR_OF_FATPTR(x94);
   uint64_t* x104 = CTYPES_ADDR_OF_FATPTR(x93);
   Hacl_Bignum256_mod_exp_consttime_precomp(x98, x99, x100, x103, x104);
   return Val_unit;
}
value _14_Hacl_Bignum256_mod_inv_prime_vartime_precomp(value x108,
                                                       value x107,
                                                       value x106)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x109 =
   CTYPES_ADDR_OF_FATPTR(x108);
   uint64_t* x110 = CTYPES_ADDR_OF_FATPTR(x107);
   uint64_t* x111 = CTYPES_ADDR_OF_FATPTR(x106);
   Hacl_Bignum256_mod_inv_prime_vartime_precomp(x109, x110, x111);
   return Val_unit;
}
value _15_Hacl_Bignum256_new_bn_from_bytes_be(value x114, value x113)
{
   uint32_t x115 = Uint32_val(x114);
   unsigned char* x118 = CTYPES_PTR_OF_OCAML_BYTES(x113);
   uint64_t* x119 = Hacl_Bignum256_new_bn_from_bytes_be(x115, x118);
   return CTYPES_FROM_PTR(x119);
}
value _16_Hacl_Bignum256_new_bn_from_bytes_le(value x121, value x120)
{
   uint32_t x122 = Uint32_val(x121);
   unsigned char* x125 = CTYPES_PTR_OF_OCAML_BYTES(x120);
   uint64_t* x126 = Hacl_Bignum256_new_bn_from_bytes_le(x122, x125);
   return CTYPES_FROM_PTR(x126);
}
value _17_Hacl_Bignum256_bn_to_bytes_be(value x128, value x127)
{
   uint64_t* x129 = CTYPES_ADDR_OF_FATPTR(x128);
   unsigned char* x130 = CTYPES_PTR_OF_OCAML_BYTES(x127);
   Hacl_Bignum256_bn_to_bytes_be(x129, x130);
   return Val_unit;
}
value _18_Hacl_Bignum256_bn_to_bytes_le(value x133, value x132)
{
   uint64_t* x134 = CTYPES_ADDR_OF_FATPTR(x133);
   unsigned char* x135 = CTYPES_PTR_OF_OCAML_BYTES(x132);
   Hacl_Bignum256_bn_to_bytes_le(x134, x135);
   return Val_unit;
}
value _19_Hacl_Bignum256_lt_mask(value x138, value x137)
{
   uint64_t* x139 = CTYPES_ADDR_OF_FATPTR(x138);
   uint64_t* x140 = CTYPES_ADDR_OF_FATPTR(x137);
   uint64_t x141 = Hacl_Bignum256_lt_mask(x139, x140);
   return integers_copy_uint64(x141);
}
