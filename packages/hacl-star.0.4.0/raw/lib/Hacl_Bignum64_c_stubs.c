
#include "Hacl_Bignum64.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Bignum64_add(value x4, value x3, value x2, value x1)
{
   uint32_t x5 = Uint32_val(x4);
   uint64_t* x8 = CTYPES_ADDR_OF_FATPTR(x3);
   uint64_t* x9 = CTYPES_ADDR_OF_FATPTR(x2);
   uint64_t* x10 = CTYPES_ADDR_OF_FATPTR(x1);
   uint64_t x11 = Hacl_Bignum64_add(x5, x8, x9, x10);
   return integers_copy_uint64(x11);
}
value _2_Hacl_Bignum64_sub(value x15, value x14, value x13, value x12)
{
   uint32_t x16 = Uint32_val(x15);
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x14);
   uint64_t* x20 = CTYPES_ADDR_OF_FATPTR(x13);
   uint64_t* x21 = CTYPES_ADDR_OF_FATPTR(x12);
   uint64_t x22 = Hacl_Bignum64_sub(x16, x19, x20, x21);
   return integers_copy_uint64(x22);
}
value _3_Hacl_Bignum64_mul(value x26, value x25, value x24, value x23)
{
   uint32_t x27 = Uint32_val(x26);
   uint64_t* x30 = CTYPES_ADDR_OF_FATPTR(x25);
   uint64_t* x31 = CTYPES_ADDR_OF_FATPTR(x24);
   uint64_t* x32 = CTYPES_ADDR_OF_FATPTR(x23);
   Hacl_Bignum64_mul(x27, x30, x31, x32);
   return Val_unit;
}
value _4_Hacl_Bignum64_sqr(value x36, value x35, value x34)
{
   uint32_t x37 = Uint32_val(x36);
   uint64_t* x40 = CTYPES_ADDR_OF_FATPTR(x35);
   uint64_t* x41 = CTYPES_ADDR_OF_FATPTR(x34);
   Hacl_Bignum64_sqr(x37, x40, x41);
   return Val_unit;
}
value _5_Hacl_Bignum64_mod(value x46, value x45, value x44, value x43)
{
   uint32_t x47 = Uint32_val(x46);
   uint64_t* x50 = CTYPES_ADDR_OF_FATPTR(x45);
   uint64_t* x51 = CTYPES_ADDR_OF_FATPTR(x44);
   uint64_t* x52 = CTYPES_ADDR_OF_FATPTR(x43);
   _Bool x53 = Hacl_Bignum64_mod(x47, x50, x51, x52);
   return Val_bool(x53);
}
value _6_Hacl_Bignum64_mod_exp_vartime(value x59, value x58, value x57,
                                       value x56, value x55, value x54)
{
   uint32_t x60 = Uint32_val(x59);
   uint64_t* x63 = CTYPES_ADDR_OF_FATPTR(x58);
   uint64_t* x64 = CTYPES_ADDR_OF_FATPTR(x57);
   uint32_t x65 = Uint32_val(x56);
   uint64_t* x68 = CTYPES_ADDR_OF_FATPTR(x55);
   uint64_t* x69 = CTYPES_ADDR_OF_FATPTR(x54);
   _Bool x70 = Hacl_Bignum64_mod_exp_vartime(x60, x63, x64, x65, x68, x69);
   return Val_bool(x70);
}
value _6_Hacl_Bignum64_mod_exp_vartime_byte6(value* argv, int argc)
{
   value x71 = argv[5];
   value x72 = argv[4];
   value x73 = argv[3];
   value x74 = argv[2];
   value x75 = argv[1];
   value x76 = argv[0];
   return _6_Hacl_Bignum64_mod_exp_vartime(x76, x75, x74, x73, x72, x71);
}
value _7_Hacl_Bignum64_mod_exp_consttime(value x82, value x81, value x80,
                                         value x79, value x78, value x77)
{
   uint32_t x83 = Uint32_val(x82);
   uint64_t* x86 = CTYPES_ADDR_OF_FATPTR(x81);
   uint64_t* x87 = CTYPES_ADDR_OF_FATPTR(x80);
   uint32_t x88 = Uint32_val(x79);
   uint64_t* x91 = CTYPES_ADDR_OF_FATPTR(x78);
   uint64_t* x92 = CTYPES_ADDR_OF_FATPTR(x77);
   _Bool x93 = Hacl_Bignum64_mod_exp_consttime(x83, x86, x87, x88, x91, x92);
   return Val_bool(x93);
}
value _7_Hacl_Bignum64_mod_exp_consttime_byte6(value* argv, int argc)
{
   value x94 = argv[5];
   value x95 = argv[4];
   value x96 = argv[3];
   value x97 = argv[2];
   value x98 = argv[1];
   value x99 = argv[0];
   return _7_Hacl_Bignum64_mod_exp_consttime(x99, x98, x97, x96, x95, x94);
}
value _8_Hacl_Bignum64_mod_inv_prime_vartime(value x103, value x102,
                                             value x101, value x100)
{
   uint32_t x104 = Uint32_val(x103);
   uint64_t* x107 = CTYPES_ADDR_OF_FATPTR(x102);
   uint64_t* x108 = CTYPES_ADDR_OF_FATPTR(x101);
   uint64_t* x109 = CTYPES_ADDR_OF_FATPTR(x100);
   _Bool x110 = Hacl_Bignum64_mod_inv_prime_vartime(x104, x107, x108, x109);
   return Val_bool(x110);
}
value _9_Hacl_Bignum64_mont_ctx_init(value x112, value x111)
{
   uint32_t x113 = Uint32_val(x112);
   uint64_t* x116 = CTYPES_ADDR_OF_FATPTR(x111);
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x117 =
   Hacl_Bignum64_mont_ctx_init(x113, x116);
   return CTYPES_FROM_PTR(x117);
}
value _10_Hacl_Bignum64_mont_ctx_free(value x118)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x119 =
   CTYPES_ADDR_OF_FATPTR(x118);
   Hacl_Bignum64_mont_ctx_free(x119);
   return Val_unit;
}
value _11_Hacl_Bignum64_mod_precomp(value x123, value x122, value x121)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x124 =
   CTYPES_ADDR_OF_FATPTR(x123);
   uint64_t* x125 = CTYPES_ADDR_OF_FATPTR(x122);
   uint64_t* x126 = CTYPES_ADDR_OF_FATPTR(x121);
   Hacl_Bignum64_mod_precomp(x124, x125, x126);
   return Val_unit;
}
value _12_Hacl_Bignum64_mod_exp_vartime_precomp(value x132, value x131,
                                                value x130, value x129,
                                                value x128)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x133 =
   CTYPES_ADDR_OF_FATPTR(x132);
   uint64_t* x134 = CTYPES_ADDR_OF_FATPTR(x131);
   uint32_t x135 = Uint32_val(x130);
   uint64_t* x138 = CTYPES_ADDR_OF_FATPTR(x129);
   uint64_t* x139 = CTYPES_ADDR_OF_FATPTR(x128);
   Hacl_Bignum64_mod_exp_vartime_precomp(x133, x134, x135, x138, x139);
   return Val_unit;
}
value _13_Hacl_Bignum64_mod_exp_consttime_precomp(value x145, value x144,
                                                  value x143, value x142,
                                                  value x141)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x146 =
   CTYPES_ADDR_OF_FATPTR(x145);
   uint64_t* x147 = CTYPES_ADDR_OF_FATPTR(x144);
   uint32_t x148 = Uint32_val(x143);
   uint64_t* x151 = CTYPES_ADDR_OF_FATPTR(x142);
   uint64_t* x152 = CTYPES_ADDR_OF_FATPTR(x141);
   Hacl_Bignum64_mod_exp_consttime_precomp(x146, x147, x148, x151, x152);
   return Val_unit;
}
value _14_Hacl_Bignum64_mod_inv_prime_vartime_precomp(value x156, value x155,
                                                      value x154)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64_s* x157 =
   CTYPES_ADDR_OF_FATPTR(x156);
   uint64_t* x158 = CTYPES_ADDR_OF_FATPTR(x155);
   uint64_t* x159 = CTYPES_ADDR_OF_FATPTR(x154);
   Hacl_Bignum64_mod_inv_prime_vartime_precomp(x157, x158, x159);
   return Val_unit;
}
value _15_Hacl_Bignum64_new_bn_from_bytes_be(value x162, value x161)
{
   uint32_t x163 = Uint32_val(x162);
   unsigned char* x166 = CTYPES_PTR_OF_OCAML_BYTES(x161);
   uint64_t* x167 = Hacl_Bignum64_new_bn_from_bytes_be(x163, x166);
   return CTYPES_FROM_PTR(x167);
}
value _16_Hacl_Bignum64_new_bn_from_bytes_le(value x169, value x168)
{
   uint32_t x170 = Uint32_val(x169);
   unsigned char* x173 = CTYPES_PTR_OF_OCAML_BYTES(x168);
   uint64_t* x174 = Hacl_Bignum64_new_bn_from_bytes_le(x170, x173);
   return CTYPES_FROM_PTR(x174);
}
value _17_Hacl_Bignum64_bn_to_bytes_be(value x177, value x176, value x175)
{
   uint32_t x178 = Uint32_val(x177);
   uint64_t* x181 = CTYPES_ADDR_OF_FATPTR(x176);
   unsigned char* x182 = CTYPES_PTR_OF_OCAML_BYTES(x175);
   Hacl_Bignum64_bn_to_bytes_be(x178, x181, x182);
   return Val_unit;
}
value _18_Hacl_Bignum64_bn_to_bytes_le(value x186, value x185, value x184)
{
   uint32_t x187 = Uint32_val(x186);
   uint64_t* x190 = CTYPES_ADDR_OF_FATPTR(x185);
   unsigned char* x191 = CTYPES_PTR_OF_OCAML_BYTES(x184);
   Hacl_Bignum64_bn_to_bytes_le(x187, x190, x191);
   return Val_unit;
}
value _19_Hacl_Bignum64_lt_mask(value x195, value x194, value x193)
{
   uint32_t x196 = Uint32_val(x195);
   uint64_t* x199 = CTYPES_ADDR_OF_FATPTR(x194);
   uint64_t* x200 = CTYPES_ADDR_OF_FATPTR(x193);
   uint64_t x201 = Hacl_Bignum64_lt_mask(x196, x199, x200);
   return integers_copy_uint64(x201);
}
