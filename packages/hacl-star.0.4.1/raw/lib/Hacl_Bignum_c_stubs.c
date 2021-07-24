
#include "Hacl_Bignum.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Bignum_Convert_bn_from_bytes_be_uint64(value x3, value x2,
                                                     value x1)
{
   uint32_t x4 = Uint32_val(x3);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint64_t* x8 = CTYPES_ADDR_OF_FATPTR(x1);
   Hacl_Bignum_Convert_bn_from_bytes_be_uint64(x4, x7, x8);
   return Val_unit;
}
value _2_Hacl_Bignum_Convert_bn_to_bytes_be_uint64(value x12, value x11,
                                                   value x10)
{
   uint32_t x13 = Uint32_val(x12);
   uint64_t* x16 = CTYPES_ADDR_OF_FATPTR(x11);
   unsigned char* x17 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   Hacl_Bignum_Convert_bn_to_bytes_be_uint64(x13, x16, x17);
   return Val_unit;
}
value _3_Hacl_Bignum_Lib_bn_get_top_index_u32(value x20, value x19)
{
   uint32_t x21 = Uint32_val(x20);
   uint32_t* x24 = CTYPES_ADDR_OF_FATPTR(x19);
   uint32_t x25 = Hacl_Bignum_Lib_bn_get_top_index_u32(x21, x24);
   return integers_copy_uint32(x25);
}
value _4_Hacl_Bignum_Lib_bn_get_top_index_u64(value x27, value x26)
{
   uint32_t x28 = Uint32_val(x27);
   uint64_t* x31 = CTYPES_ADDR_OF_FATPTR(x26);
   uint64_t x32 = Hacl_Bignum_Lib_bn_get_top_index_u64(x28, x31);
   return integers_copy_uint64(x32);
}
value _5_Hacl_Bignum_Addition_bn_sub_eq_len_u32(value x36, value x35,
                                                value x34, value x33)
{
   uint32_t x37 = Uint32_val(x36);
   uint32_t* x40 = CTYPES_ADDR_OF_FATPTR(x35);
   uint32_t* x41 = CTYPES_ADDR_OF_FATPTR(x34);
   uint32_t* x42 = CTYPES_ADDR_OF_FATPTR(x33);
   uint32_t x43 = Hacl_Bignum_Addition_bn_sub_eq_len_u32(x37, x40, x41, x42);
   return integers_copy_uint32(x43);
}
value _6_Hacl_Bignum_Addition_bn_sub_eq_len_u64(value x47, value x46,
                                                value x45, value x44)
{
   uint32_t x48 = Uint32_val(x47);
   uint64_t* x51 = CTYPES_ADDR_OF_FATPTR(x46);
   uint64_t* x52 = CTYPES_ADDR_OF_FATPTR(x45);
   uint64_t* x53 = CTYPES_ADDR_OF_FATPTR(x44);
   uint64_t x54 = Hacl_Bignum_Addition_bn_sub_eq_len_u64(x48, x51, x52, x53);
   return integers_copy_uint64(x54);
}
value _7_Hacl_Bignum_Addition_bn_add_eq_len_u32(value x58, value x57,
                                                value x56, value x55)
{
   uint32_t x59 = Uint32_val(x58);
   uint32_t* x62 = CTYPES_ADDR_OF_FATPTR(x57);
   uint32_t* x63 = CTYPES_ADDR_OF_FATPTR(x56);
   uint32_t* x64 = CTYPES_ADDR_OF_FATPTR(x55);
   uint32_t x65 = Hacl_Bignum_Addition_bn_add_eq_len_u32(x59, x62, x63, x64);
   return integers_copy_uint32(x65);
}
value _8_Hacl_Bignum_Addition_bn_add_eq_len_u64(value x69, value x68,
                                                value x67, value x66)
{
   uint32_t x70 = Uint32_val(x69);
   uint64_t* x73 = CTYPES_ADDR_OF_FATPTR(x68);
   uint64_t* x74 = CTYPES_ADDR_OF_FATPTR(x67);
   uint64_t* x75 = CTYPES_ADDR_OF_FATPTR(x66);
   uint64_t x76 = Hacl_Bignum_Addition_bn_add_eq_len_u64(x70, x73, x74, x75);
   return integers_copy_uint64(x76);
}
value _9_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(value x81, value x80,
                                                       value x79, value x78,
                                                       value x77)
{
   uint32_t x82 = Uint32_val(x81);
   uint32_t* x85 = CTYPES_ADDR_OF_FATPTR(x80);
   uint32_t* x86 = CTYPES_ADDR_OF_FATPTR(x79);
   uint32_t* x87 = CTYPES_ADDR_OF_FATPTR(x78);
   uint32_t* x88 = CTYPES_ADDR_OF_FATPTR(x77);
   Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(x82, x85, x86, x87, x88);
   return Val_unit;
}
value _10_Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(value x94, value x93,
                                                        value x92, value x91,
                                                        value x90)
{
   uint32_t x95 = Uint32_val(x94);
   uint64_t* x98 = CTYPES_ADDR_OF_FATPTR(x93);
   uint64_t* x99 = CTYPES_ADDR_OF_FATPTR(x92);
   uint64_t* x100 = CTYPES_ADDR_OF_FATPTR(x91);
   uint64_t* x101 = CTYPES_ADDR_OF_FATPTR(x90);
   Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(x95, x98, x99, x100, x101);
   return Val_unit;
}
value _11_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(value x106,
                                                        value x105,
                                                        value x104,
                                                        value x103)
{
   uint32_t x107 = Uint32_val(x106);
   uint32_t* x110 = CTYPES_ADDR_OF_FATPTR(x105);
   uint32_t* x111 = CTYPES_ADDR_OF_FATPTR(x104);
   uint32_t* x112 = CTYPES_ADDR_OF_FATPTR(x103);
   Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(x107, x110, x111, x112);
   return Val_unit;
}
value _12_Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(value x117,
                                                        value x116,
                                                        value x115,
                                                        value x114)
{
   uint32_t x118 = Uint32_val(x117);
   uint64_t* x121 = CTYPES_ADDR_OF_FATPTR(x116);
   uint64_t* x122 = CTYPES_ADDR_OF_FATPTR(x115);
   uint64_t* x123 = CTYPES_ADDR_OF_FATPTR(x114);
   Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(x118, x121, x122, x123);
   return Val_unit;
}
value _13_Hacl_Bignum_bn_add_mod_n_u32(value x129, value x128, value x127,
                                       value x126, value x125)
{
   uint32_t x130 = Uint32_val(x129);
   uint32_t* x133 = CTYPES_ADDR_OF_FATPTR(x128);
   uint32_t* x134 = CTYPES_ADDR_OF_FATPTR(x127);
   uint32_t* x135 = CTYPES_ADDR_OF_FATPTR(x126);
   uint32_t* x136 = CTYPES_ADDR_OF_FATPTR(x125);
   Hacl_Bignum_bn_add_mod_n_u32(x130, x133, x134, x135, x136);
   return Val_unit;
}
value _14_Hacl_Bignum_bn_add_mod_n_u64(value x142, value x141, value x140,
                                       value x139, value x138)
{
   uint32_t x143 = Uint32_val(x142);
   uint64_t* x146 = CTYPES_ADDR_OF_FATPTR(x141);
   uint64_t* x147 = CTYPES_ADDR_OF_FATPTR(x140);
   uint64_t* x148 = CTYPES_ADDR_OF_FATPTR(x139);
   uint64_t* x149 = CTYPES_ADDR_OF_FATPTR(x138);
   Hacl_Bignum_bn_add_mod_n_u64(x143, x146, x147, x148, x149);
   return Val_unit;
}
value _15_Hacl_Bignum_ModInvLimb_mod_inv_uint32(value x151)
{
   uint32_t x152 = Uint32_val(x151);
   uint32_t x155 = Hacl_Bignum_ModInvLimb_mod_inv_uint32(x152);
   return integers_copy_uint32(x155);
}
value _16_Hacl_Bignum_ModInvLimb_mod_inv_uint64(value x156)
{
   uint64_t x157 = Uint64_val(x156);
   uint64_t x160 = Hacl_Bignum_ModInvLimb_mod_inv_uint64(x157);
   return integers_copy_uint64(x160);
}
value _17_Hacl_Bignum_Montgomery_bn_check_modulus_u32(value x162, value x161)
{
   uint32_t x163 = Uint32_val(x162);
   uint32_t* x166 = CTYPES_ADDR_OF_FATPTR(x161);
   uint32_t x167 = Hacl_Bignum_Montgomery_bn_check_modulus_u32(x163, x166);
   return integers_copy_uint32(x167);
}
value _18_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32(value x171,
                                                         value x170,
                                                         value x169,
                                                         value x168)
{
   uint32_t x172 = Uint32_val(x171);
   uint32_t x175 = Uint32_val(x170);
   uint32_t* x178 = CTYPES_ADDR_OF_FATPTR(x169);
   uint32_t* x179 = CTYPES_ADDR_OF_FATPTR(x168);
   Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32(x172, x175, x178, x179);
   return Val_unit;
}
value _19_Hacl_Bignum_Montgomery_bn_mont_reduction_u32(value x185,
                                                       value x184,
                                                       value x183,
                                                       value x182,
                                                       value x181)
{
   uint32_t x186 = Uint32_val(x185);
   uint32_t* x189 = CTYPES_ADDR_OF_FATPTR(x184);
   uint32_t x190 = Uint32_val(x183);
   uint32_t* x193 = CTYPES_ADDR_OF_FATPTR(x182);
   uint32_t* x194 = CTYPES_ADDR_OF_FATPTR(x181);
   Hacl_Bignum_Montgomery_bn_mont_reduction_u32(x186, x189, x190, x193, x194);
   return Val_unit;
}
value _20_Hacl_Bignum_Montgomery_bn_to_mont_u32(value x201, value x200,
                                                value x199, value x198,
                                                value x197, value x196)
{
   uint32_t x202 = Uint32_val(x201);
   uint32_t* x205 = CTYPES_ADDR_OF_FATPTR(x200);
   uint32_t x206 = Uint32_val(x199);
   uint32_t* x209 = CTYPES_ADDR_OF_FATPTR(x198);
   uint32_t* x210 = CTYPES_ADDR_OF_FATPTR(x197);
   uint32_t* x211 = CTYPES_ADDR_OF_FATPTR(x196);
   Hacl_Bignum_Montgomery_bn_to_mont_u32(x202, x205, x206, x209, x210, x211);
   return Val_unit;
}
value _20_Hacl_Bignum_Montgomery_bn_to_mont_u32_byte6(value* argv, int argc)
{
   value x213 = argv[5];
   value x214 = argv[4];
   value x215 = argv[3];
   value x216 = argv[2];
   value x217 = argv[1];
   value x218 = argv[0];
   return
     _20_Hacl_Bignum_Montgomery_bn_to_mont_u32(x218, x217, x216, x215, 
                                               x214, x213);
}
value _21_Hacl_Bignum_Montgomery_bn_from_mont_u32(value x223, value x222,
                                                  value x221, value x220,
                                                  value x219)
{
   uint32_t x224 = Uint32_val(x223);
   uint32_t* x227 = CTYPES_ADDR_OF_FATPTR(x222);
   uint32_t x228 = Uint32_val(x221);
   uint32_t* x231 = CTYPES_ADDR_OF_FATPTR(x220);
   uint32_t* x232 = CTYPES_ADDR_OF_FATPTR(x219);
   Hacl_Bignum_Montgomery_bn_from_mont_u32(x224, x227, x228, x231, x232);
   return Val_unit;
}
value _22_Hacl_Bignum_Montgomery_bn_mont_mul_u32(value x239, value x238,
                                                 value x237, value x236,
                                                 value x235, value x234)
{
   uint32_t x240 = Uint32_val(x239);
   uint32_t* x243 = CTYPES_ADDR_OF_FATPTR(x238);
   uint32_t x244 = Uint32_val(x237);
   uint32_t* x247 = CTYPES_ADDR_OF_FATPTR(x236);
   uint32_t* x248 = CTYPES_ADDR_OF_FATPTR(x235);
   uint32_t* x249 = CTYPES_ADDR_OF_FATPTR(x234);
   Hacl_Bignum_Montgomery_bn_mont_mul_u32(x240, x243, x244, x247, x248, x249);
   return Val_unit;
}
value _22_Hacl_Bignum_Montgomery_bn_mont_mul_u32_byte6(value* argv, int argc)
{
   value x251 = argv[5];
   value x252 = argv[4];
   value x253 = argv[3];
   value x254 = argv[2];
   value x255 = argv[1];
   value x256 = argv[0];
   return
     _22_Hacl_Bignum_Montgomery_bn_mont_mul_u32(x256, x255, x254, x253, 
                                                x252, x251);
}
value _23_Hacl_Bignum_Montgomery_bn_mont_sqr_u32(value x261, value x260,
                                                 value x259, value x258,
                                                 value x257)
{
   uint32_t x262 = Uint32_val(x261);
   uint32_t* x265 = CTYPES_ADDR_OF_FATPTR(x260);
   uint32_t x266 = Uint32_val(x259);
   uint32_t* x269 = CTYPES_ADDR_OF_FATPTR(x258);
   uint32_t* x270 = CTYPES_ADDR_OF_FATPTR(x257);
   Hacl_Bignum_Montgomery_bn_mont_sqr_u32(x262, x265, x266, x269, x270);
   return Val_unit;
}
value _24_Hacl_Bignum_Montgomery_bn_check_modulus_u64(value x273, value x272)
{
   uint32_t x274 = Uint32_val(x273);
   uint64_t* x277 = CTYPES_ADDR_OF_FATPTR(x272);
   uint64_t x278 = Hacl_Bignum_Montgomery_bn_check_modulus_u64(x274, x277);
   return integers_copy_uint64(x278);
}
value _25_Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64(value x282,
                                                         value x281,
                                                         value x280,
                                                         value x279)
{
   uint32_t x283 = Uint32_val(x282);
   uint32_t x286 = Uint32_val(x281);
   uint64_t* x289 = CTYPES_ADDR_OF_FATPTR(x280);
   uint64_t* x290 = CTYPES_ADDR_OF_FATPTR(x279);
   Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64(x283, x286, x289, x290);
   return Val_unit;
}
value _26_Hacl_Bignum_Montgomery_bn_mont_reduction_u64(value x296,
                                                       value x295,
                                                       value x294,
                                                       value x293,
                                                       value x292)
{
   uint32_t x297 = Uint32_val(x296);
   uint64_t* x300 = CTYPES_ADDR_OF_FATPTR(x295);
   uint64_t x301 = Uint64_val(x294);
   uint64_t* x304 = CTYPES_ADDR_OF_FATPTR(x293);
   uint64_t* x305 = CTYPES_ADDR_OF_FATPTR(x292);
   Hacl_Bignum_Montgomery_bn_mont_reduction_u64(x297, x300, x301, x304, x305);
   return Val_unit;
}
value _27_Hacl_Bignum_Montgomery_bn_to_mont_u64(value x312, value x311,
                                                value x310, value x309,
                                                value x308, value x307)
{
   uint32_t x313 = Uint32_val(x312);
   uint64_t* x316 = CTYPES_ADDR_OF_FATPTR(x311);
   uint64_t x317 = Uint64_val(x310);
   uint64_t* x320 = CTYPES_ADDR_OF_FATPTR(x309);
   uint64_t* x321 = CTYPES_ADDR_OF_FATPTR(x308);
   uint64_t* x322 = CTYPES_ADDR_OF_FATPTR(x307);
   Hacl_Bignum_Montgomery_bn_to_mont_u64(x313, x316, x317, x320, x321, x322);
   return Val_unit;
}
value _27_Hacl_Bignum_Montgomery_bn_to_mont_u64_byte6(value* argv, int argc)
{
   value x324 = argv[5];
   value x325 = argv[4];
   value x326 = argv[3];
   value x327 = argv[2];
   value x328 = argv[1];
   value x329 = argv[0];
   return
     _27_Hacl_Bignum_Montgomery_bn_to_mont_u64(x329, x328, x327, x326, 
                                               x325, x324);
}
value _28_Hacl_Bignum_Montgomery_bn_from_mont_u64(value x334, value x333,
                                                  value x332, value x331,
                                                  value x330)
{
   uint32_t x335 = Uint32_val(x334);
   uint64_t* x338 = CTYPES_ADDR_OF_FATPTR(x333);
   uint64_t x339 = Uint64_val(x332);
   uint64_t* x342 = CTYPES_ADDR_OF_FATPTR(x331);
   uint64_t* x343 = CTYPES_ADDR_OF_FATPTR(x330);
   Hacl_Bignum_Montgomery_bn_from_mont_u64(x335, x338, x339, x342, x343);
   return Val_unit;
}
value _29_Hacl_Bignum_Montgomery_bn_mont_mul_u64(value x350, value x349,
                                                 value x348, value x347,
                                                 value x346, value x345)
{
   uint32_t x351 = Uint32_val(x350);
   uint64_t* x354 = CTYPES_ADDR_OF_FATPTR(x349);
   uint64_t x355 = Uint64_val(x348);
   uint64_t* x358 = CTYPES_ADDR_OF_FATPTR(x347);
   uint64_t* x359 = CTYPES_ADDR_OF_FATPTR(x346);
   uint64_t* x360 = CTYPES_ADDR_OF_FATPTR(x345);
   Hacl_Bignum_Montgomery_bn_mont_mul_u64(x351, x354, x355, x358, x359, x360);
   return Val_unit;
}
value _29_Hacl_Bignum_Montgomery_bn_mont_mul_u64_byte6(value* argv, int argc)
{
   value x362 = argv[5];
   value x363 = argv[4];
   value x364 = argv[3];
   value x365 = argv[2];
   value x366 = argv[1];
   value x367 = argv[0];
   return
     _29_Hacl_Bignum_Montgomery_bn_mont_mul_u64(x367, x366, x365, x364, 
                                                x363, x362);
}
value _30_Hacl_Bignum_Montgomery_bn_mont_sqr_u64(value x372, value x371,
                                                 value x370, value x369,
                                                 value x368)
{
   uint32_t x373 = Uint32_val(x372);
   uint64_t* x376 = CTYPES_ADDR_OF_FATPTR(x371);
   uint64_t x377 = Uint64_val(x370);
   uint64_t* x380 = CTYPES_ADDR_OF_FATPTR(x369);
   uint64_t* x381 = CTYPES_ADDR_OF_FATPTR(x368);
   Hacl_Bignum_Montgomery_bn_mont_sqr_u64(x373, x376, x377, x380, x381);
   return Val_unit;
}
value _31_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32(value x387,
                                                          value x386,
                                                          value x385,
                                                          value x384,
                                                          value x383)
{
   uint32_t x388 = Uint32_val(x387);
   uint32_t* x391 = CTYPES_ADDR_OF_FATPTR(x386);
   uint32_t* x392 = CTYPES_ADDR_OF_FATPTR(x385);
   uint32_t x393 = Uint32_val(x384);
   uint32_t* x396 = CTYPES_ADDR_OF_FATPTR(x383);
   uint32_t x397 =
   Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32(x388, x391, x392, 
                                                   x393, x396);
   return integers_copy_uint32(x397);
}
value _32_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(value x405,
                                                                    value x404,
                                                                    value x403,
                                                                    value x402,
                                                                    value x401,
                                                                    value x400,
                                                                    value x399,
                                                                    value x398)
{
   uint32_t x406 = Uint32_val(x405);
   uint32_t* x409 = CTYPES_ADDR_OF_FATPTR(x404);
   uint32_t x410 = Uint32_val(x403);
   uint32_t* x413 = CTYPES_ADDR_OF_FATPTR(x402);
   uint32_t* x414 = CTYPES_ADDR_OF_FATPTR(x401);
   uint32_t x415 = Uint32_val(x400);
   uint32_t* x418 = CTYPES_ADDR_OF_FATPTR(x399);
   uint32_t* x419 = CTYPES_ADDR_OF_FATPTR(x398);
   Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(x406, x409,
                                                             x410, x413,
                                                             x414, x415,
                                                             x418, x419);
   return Val_unit;
}
value _32_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32_byte8
     (value* argv, int argc)
{
   value x421 = argv[7];
   value x422 = argv[6];
   value x423 = argv[5];
   value x424 = argv[4];
   value x425 = argv[3];
   value x426 = argv[2];
   value x427 = argv[1];
   value x428 = argv[0];
   return
     _32_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(x428,
                                                                   x427,
                                                                   x426,
                                                                   x425,
                                                                   x424,
                                                                   x423,
                                                                   x422,
                                                                   x421);
}
value _33_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32
     (value x436, value x435, value x434, value x433, value x432, value x431,
      value x430, value x429)
{
   uint32_t x437 = Uint32_val(x436);
   uint32_t* x440 = CTYPES_ADDR_OF_FATPTR(x435);
   uint32_t x441 = Uint32_val(x434);
   uint32_t* x444 = CTYPES_ADDR_OF_FATPTR(x433);
   uint32_t* x445 = CTYPES_ADDR_OF_FATPTR(x432);
   uint32_t x446 = Uint32_val(x431);
   uint32_t* x449 = CTYPES_ADDR_OF_FATPTR(x430);
   uint32_t* x450 = CTYPES_ADDR_OF_FATPTR(x429);
   Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32(x437, 
                                                               x440, 
                                                               x441, 
                                                               x444, 
                                                               x445, 
                                                               x446, 
                                                               x449, 
                                                               x450);
   return Val_unit;
}
value _33_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32_byte8
     (value* argv, int argc)
{
   value x452 = argv[7];
   value x453 = argv[6];
   value x454 = argv[5];
   value x455 = argv[4];
   value x456 = argv[3];
   value x457 = argv[2];
   value x458 = argv[1];
   value x459 = argv[0];
   return
     _33_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32(
     x459, x458, x457, x456, x455, x454, x453, x452);
}
value _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32(value x466,
                                                            value x465,
                                                            value x464,
                                                            value x463,
                                                            value x462,
                                                            value x461,
                                                            value x460)
{
   uint32_t x467 = Uint32_val(x466);
   uint32_t x470 = Uint32_val(x465);
   uint32_t* x473 = CTYPES_ADDR_OF_FATPTR(x464);
   uint32_t* x474 = CTYPES_ADDR_OF_FATPTR(x463);
   uint32_t x475 = Uint32_val(x462);
   uint32_t* x478 = CTYPES_ADDR_OF_FATPTR(x461);
   uint32_t* x479 = CTYPES_ADDR_OF_FATPTR(x460);
   Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32(x467, x470, x473, 
                                                     x474, x475, x478, 
                                                     x479);
   return Val_unit;
}
value _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32_byte7(value* argv,
                                                                  int argc)
{
   value x481 = argv[6];
   value x482 = argv[5];
   value x483 = argv[4];
   value x484 = argv[3];
   value x485 = argv[2];
   value x486 = argv[1];
   value x487 = argv[0];
   return
     _34_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32(x487, x486, 
                                                           x485, x484, 
                                                           x483, x482, 
                                                           x481);
}
value _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32(value x494,
                                                              value x493,
                                                              value x492,
                                                              value x491,
                                                              value x490,
                                                              value x489,
                                                              value x488)
{
   uint32_t x495 = Uint32_val(x494);
   uint32_t x498 = Uint32_val(x493);
   uint32_t* x501 = CTYPES_ADDR_OF_FATPTR(x492);
   uint32_t* x502 = CTYPES_ADDR_OF_FATPTR(x491);
   uint32_t x503 = Uint32_val(x490);
   uint32_t* x506 = CTYPES_ADDR_OF_FATPTR(x489);
   uint32_t* x507 = CTYPES_ADDR_OF_FATPTR(x488);
   Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32(x495, x498, x501,
                                                       x502, x503, x506,
                                                       x507);
   return Val_unit;
}
value _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32_byte7(value* argv,
                                                                    int argc)
{
   value x509 = argv[6];
   value x510 = argv[5];
   value x511 = argv[4];
   value x512 = argv[3];
   value x513 = argv[2];
   value x514 = argv[1];
   value x515 = argv[0];
   return
     _35_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32(x515, x514,
                                                             x513, x512,
                                                             x511, x510,
                                                             x509);
}
value _36_Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64(value x520,
                                                          value x519,
                                                          value x518,
                                                          value x517,
                                                          value x516)
{
   uint32_t x521 = Uint32_val(x520);
   uint64_t* x524 = CTYPES_ADDR_OF_FATPTR(x519);
   uint64_t* x525 = CTYPES_ADDR_OF_FATPTR(x518);
   uint32_t x526 = Uint32_val(x517);
   uint64_t* x529 = CTYPES_ADDR_OF_FATPTR(x516);
   uint64_t x530 =
   Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64(x521, x524, x525, 
                                                   x526, x529);
   return integers_copy_uint64(x530);
}
value _37_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(value x538,
                                                                    value x537,
                                                                    value x536,
                                                                    value x535,
                                                                    value x534,
                                                                    value x533,
                                                                    value x532,
                                                                    value x531)
{
   uint32_t x539 = Uint32_val(x538);
   uint64_t* x542 = CTYPES_ADDR_OF_FATPTR(x537);
   uint64_t x543 = Uint64_val(x536);
   uint64_t* x546 = CTYPES_ADDR_OF_FATPTR(x535);
   uint64_t* x547 = CTYPES_ADDR_OF_FATPTR(x534);
   uint32_t x548 = Uint32_val(x533);
   uint64_t* x551 = CTYPES_ADDR_OF_FATPTR(x532);
   uint64_t* x552 = CTYPES_ADDR_OF_FATPTR(x531);
   Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(x539, x542,
                                                             x543, x546,
                                                             x547, x548,
                                                             x551, x552);
   return Val_unit;
}
value _37_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64_byte8
     (value* argv, int argc)
{
   value x554 = argv[7];
   value x555 = argv[6];
   value x556 = argv[5];
   value x557 = argv[4];
   value x558 = argv[3];
   value x559 = argv[2];
   value x560 = argv[1];
   value x561 = argv[0];
   return
     _37_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(x561,
                                                                   x560,
                                                                   x559,
                                                                   x558,
                                                                   x557,
                                                                   x556,
                                                                   x555,
                                                                   x554);
}
value _38_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64
     (value x569, value x568, value x567, value x566, value x565, value x564,
      value x563, value x562)
{
   uint32_t x570 = Uint32_val(x569);
   uint64_t* x573 = CTYPES_ADDR_OF_FATPTR(x568);
   uint64_t x574 = Uint64_val(x567);
   uint64_t* x577 = CTYPES_ADDR_OF_FATPTR(x566);
   uint64_t* x578 = CTYPES_ADDR_OF_FATPTR(x565);
   uint32_t x579 = Uint32_val(x564);
   uint64_t* x582 = CTYPES_ADDR_OF_FATPTR(x563);
   uint64_t* x583 = CTYPES_ADDR_OF_FATPTR(x562);
   Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64(x570, 
                                                               x573, 
                                                               x574, 
                                                               x577, 
                                                               x578, 
                                                               x579, 
                                                               x582, 
                                                               x583);
   return Val_unit;
}
value _38_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64_byte8
     (value* argv, int argc)
{
   value x585 = argv[7];
   value x586 = argv[6];
   value x587 = argv[5];
   value x588 = argv[4];
   value x589 = argv[3];
   value x590 = argv[2];
   value x591 = argv[1];
   value x592 = argv[0];
   return
     _38_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64(
     x592, x591, x590, x589, x588, x587, x586, x585);
}
value _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64(value x599,
                                                            value x598,
                                                            value x597,
                                                            value x596,
                                                            value x595,
                                                            value x594,
                                                            value x593)
{
   uint32_t x600 = Uint32_val(x599);
   uint32_t x603 = Uint32_val(x598);
   uint64_t* x606 = CTYPES_ADDR_OF_FATPTR(x597);
   uint64_t* x607 = CTYPES_ADDR_OF_FATPTR(x596);
   uint32_t x608 = Uint32_val(x595);
   uint64_t* x611 = CTYPES_ADDR_OF_FATPTR(x594);
   uint64_t* x612 = CTYPES_ADDR_OF_FATPTR(x593);
   Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64(x600, x603, x606, 
                                                     x607, x608, x611, 
                                                     x612);
   return Val_unit;
}
value _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64_byte7(value* argv,
                                                                  int argc)
{
   value x614 = argv[6];
   value x615 = argv[5];
   value x616 = argv[4];
   value x617 = argv[3];
   value x618 = argv[2];
   value x619 = argv[1];
   value x620 = argv[0];
   return
     _39_Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64(x620, x619, 
                                                           x618, x617, 
                                                           x616, x615, 
                                                           x614);
}
value _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64(value x627,
                                                              value x626,
                                                              value x625,
                                                              value x624,
                                                              value x623,
                                                              value x622,
                                                              value x621)
{
   uint32_t x628 = Uint32_val(x627);
   uint32_t x631 = Uint32_val(x626);
   uint64_t* x634 = CTYPES_ADDR_OF_FATPTR(x625);
   uint64_t* x635 = CTYPES_ADDR_OF_FATPTR(x624);
   uint32_t x636 = Uint32_val(x623);
   uint64_t* x639 = CTYPES_ADDR_OF_FATPTR(x622);
   uint64_t* x640 = CTYPES_ADDR_OF_FATPTR(x621);
   Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64(x628, x631, x634,
                                                       x635, x636, x639,
                                                       x640);
   return Val_unit;
}
value _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64_byte7(value* argv,
                                                                    int argc)
{
   value x642 = argv[6];
   value x643 = argv[5];
   value x644 = argv[4];
   value x645 = argv[3];
   value x646 = argv[2];
   value x647 = argv[1];
   value x648 = argv[0];
   return
     _40_Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64(x648, x647,
                                                             x646, x645,
                                                             x644, x643,
                                                             x642);
}
