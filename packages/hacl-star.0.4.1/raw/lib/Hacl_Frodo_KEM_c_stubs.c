
#include "Hacl_Frodo_KEM.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Keccak_shake128_4x(value x10, value x9, value x8, value x7,
                                 value x6, value x5, value x4, value x3,
                                 value x2, value x1)
{
   uint32_t x11 = Uint32_val(x10);
   unsigned char* x14 = CTYPES_PTR_OF_OCAML_BYTES(x9);
   unsigned char* x15 = CTYPES_PTR_OF_OCAML_BYTES(x8);
   unsigned char* x16 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   unsigned char* x17 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   uint32_t x18 = Uint32_val(x5);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   unsigned char* x24 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   Hacl_Keccak_shake128_4x(x11, x14, x15, x16, x17, x18, x21, x22, x23, x24);
   return Val_unit;
}
value _1_Hacl_Keccak_shake128_4x_byte10(value* argv, int argc)
{
   value x26 = argv[9];
   value x27 = argv[8];
   value x28 = argv[7];
   value x29 = argv[6];
   value x30 = argv[5];
   value x31 = argv[4];
   value x32 = argv[3];
   value x33 = argv[2];
   value x34 = argv[1];
   value x35 = argv[0];
   return
     _1_Hacl_Keccak_shake128_4x(x35, x34, x33, x32, x31, x30, x29, x28, 
                                x27, x26);
}
value _2_Hacl_Impl_Matrix_mod_pow2(value x39, value x38, value x37,
                                   value x36)
{
   uint32_t x40 = Uint32_val(x39);
   uint32_t x43 = Uint32_val(x38);
   uint32_t x46 = Uint32_val(x37);
   uint16_t* x49 = CTYPES_ADDR_OF_FATPTR(x36);
   Hacl_Impl_Matrix_mod_pow2(x40, x43, x46, x49);
   return Val_unit;
}
value _3_Hacl_Impl_Matrix_matrix_add(value x54, value x53, value x52,
                                     value x51)
{
   uint32_t x55 = Uint32_val(x54);
   uint32_t x58 = Uint32_val(x53);
   uint16_t* x61 = CTYPES_ADDR_OF_FATPTR(x52);
   uint16_t* x62 = CTYPES_ADDR_OF_FATPTR(x51);
   Hacl_Impl_Matrix_matrix_add(x55, x58, x61, x62);
   return Val_unit;
}
value _4_Hacl_Impl_Matrix_matrix_sub(value x67, value x66, value x65,
                                     value x64)
{
   uint32_t x68 = Uint32_val(x67);
   uint32_t x71 = Uint32_val(x66);
   uint16_t* x74 = CTYPES_ADDR_OF_FATPTR(x65);
   uint16_t* x75 = CTYPES_ADDR_OF_FATPTR(x64);
   Hacl_Impl_Matrix_matrix_sub(x68, x71, x74, x75);
   return Val_unit;
}
value _5_Hacl_Impl_Matrix_matrix_mul(value x82, value x81, value x80,
                                     value x79, value x78, value x77)
{
   uint32_t x83 = Uint32_val(x82);
   uint32_t x86 = Uint32_val(x81);
   uint32_t x89 = Uint32_val(x80);
   uint16_t* x92 = CTYPES_ADDR_OF_FATPTR(x79);
   uint16_t* x93 = CTYPES_ADDR_OF_FATPTR(x78);
   uint16_t* x94 = CTYPES_ADDR_OF_FATPTR(x77);
   Hacl_Impl_Matrix_matrix_mul(x83, x86, x89, x92, x93, x94);
   return Val_unit;
}
value _5_Hacl_Impl_Matrix_matrix_mul_byte6(value* argv, int argc)
{
   value x96 = argv[5];
   value x97 = argv[4];
   value x98 = argv[3];
   value x99 = argv[2];
   value x100 = argv[1];
   value x101 = argv[0];
   return _5_Hacl_Impl_Matrix_matrix_mul(x101, x100, x99, x98, x97, x96);
}
value _6_Hacl_Impl_Matrix_matrix_mul_s(value x107, value x106, value x105,
                                       value x104, value x103, value x102)
{
   uint32_t x108 = Uint32_val(x107);
   uint32_t x111 = Uint32_val(x106);
   uint32_t x114 = Uint32_val(x105);
   uint16_t* x117 = CTYPES_ADDR_OF_FATPTR(x104);
   uint16_t* x118 = CTYPES_ADDR_OF_FATPTR(x103);
   uint16_t* x119 = CTYPES_ADDR_OF_FATPTR(x102);
   Hacl_Impl_Matrix_matrix_mul_s(x108, x111, x114, x117, x118, x119);
   return Val_unit;
}
value _6_Hacl_Impl_Matrix_matrix_mul_s_byte6(value* argv, int argc)
{
   value x121 = argv[5];
   value x122 = argv[4];
   value x123 = argv[3];
   value x124 = argv[2];
   value x125 = argv[1];
   value x126 = argv[0];
   return
     _6_Hacl_Impl_Matrix_matrix_mul_s(x126, x125, x124, x123, x122, x121);
}
value _7_Hacl_Impl_Matrix_matrix_eq(value x130, value x129, value x128,
                                    value x127)
{
   uint32_t x131 = Uint32_val(x130);
   uint32_t x134 = Uint32_val(x129);
   uint16_t* x137 = CTYPES_ADDR_OF_FATPTR(x128);
   uint16_t* x138 = CTYPES_ADDR_OF_FATPTR(x127);
   uint16_t x139 = Hacl_Impl_Matrix_matrix_eq(x131, x134, x137, x138);
   return Integers_val_uint16(x139);
}
value _8_Hacl_Impl_Matrix_matrix_to_lbytes(value x143, value x142,
                                           value x141, value x140)
{
   uint32_t x144 = Uint32_val(x143);
   uint32_t x147 = Uint32_val(x142);
   uint16_t* x150 = CTYPES_ADDR_OF_FATPTR(x141);
   unsigned char* x151 = CTYPES_PTR_OF_OCAML_BYTES(x140);
   Hacl_Impl_Matrix_matrix_to_lbytes(x144, x147, x150, x151);
   return Val_unit;
}
value _9_Hacl_Impl_Matrix_matrix_from_lbytes(value x156, value x155,
                                             value x154, value x153)
{
   uint32_t x157 = Uint32_val(x156);
   uint32_t x160 = Uint32_val(x155);
   unsigned char* x163 = CTYPES_PTR_OF_OCAML_BYTES(x154);
   uint16_t* x164 = CTYPES_ADDR_OF_FATPTR(x153);
   Hacl_Impl_Matrix_matrix_from_lbytes(x157, x160, x163, x164);
   return Val_unit;
}
value _10_Hacl_Impl_Frodo_Gen_frodo_gen_matrix_shake_4x(value x168,
                                                        value x167,
                                                        value x166)
{
   uint32_t x169 = Uint32_val(x168);
   unsigned char* x172 = CTYPES_PTR_OF_OCAML_BYTES(x167);
   uint16_t* x173 = CTYPES_ADDR_OF_FATPTR(x166);
   Hacl_Impl_Frodo_Gen_frodo_gen_matrix_shake_4x(x169, x172, x173);
   return Val_unit;
}
value _11_Hacl_Impl_Frodo_Params_frodo_gen_matrix(value x178, value x177,
                                                  value x176, value x175)
{
   uint8_t x179 = Uint8_val(x178);
   uint32_t x182 = Uint32_val(x177);
   unsigned char* x185 = CTYPES_PTR_OF_OCAML_BYTES(x176);
   uint16_t* x186 = CTYPES_ADDR_OF_FATPTR(x175);
   Hacl_Impl_Frodo_Params_frodo_gen_matrix(x179, x182, x185, x186);
   return Val_unit;
}
value _12_Hacl_Impl_Frodo_Sample_frodo_sample_matrix64(value x191,
                                                       value x190,
                                                       value x189,
                                                       value x188)
{
   uint32_t x192 = Uint32_val(x191);
   uint32_t x195 = Uint32_val(x190);
   unsigned char* x198 = CTYPES_PTR_OF_OCAML_BYTES(x189);
   uint16_t* x199 = CTYPES_ADDR_OF_FATPTR(x188);
   Hacl_Impl_Frodo_Sample_frodo_sample_matrix64(x192, x195, x198, x199);
   return Val_unit;
}
value _13_Hacl_Impl_Frodo_Sample_frodo_sample_matrix640(value x204,
                                                        value x203,
                                                        value x202,
                                                        value x201)
{
   uint32_t x205 = Uint32_val(x204);
   uint32_t x208 = Uint32_val(x203);
   unsigned char* x211 = CTYPES_PTR_OF_OCAML_BYTES(x202);
   uint16_t* x212 = CTYPES_ADDR_OF_FATPTR(x201);
   Hacl_Impl_Frodo_Sample_frodo_sample_matrix640(x205, x208, x211, x212);
   return Val_unit;
}
value _14_Hacl_Impl_Frodo_Sample_frodo_sample_matrix976(value x217,
                                                        value x216,
                                                        value x215,
                                                        value x214)
{
   uint32_t x218 = Uint32_val(x217);
   uint32_t x221 = Uint32_val(x216);
   unsigned char* x224 = CTYPES_PTR_OF_OCAML_BYTES(x215);
   uint16_t* x225 = CTYPES_ADDR_OF_FATPTR(x214);
   Hacl_Impl_Frodo_Sample_frodo_sample_matrix976(x218, x221, x224, x225);
   return Val_unit;
}
value _15_Hacl_Impl_Frodo_Sample_frodo_sample_matrix1344(value x230,
                                                         value x229,
                                                         value x228,
                                                         value x227)
{
   uint32_t x231 = Uint32_val(x230);
   uint32_t x234 = Uint32_val(x229);
   unsigned char* x237 = CTYPES_PTR_OF_OCAML_BYTES(x228);
   uint16_t* x238 = CTYPES_ADDR_OF_FATPTR(x227);
   Hacl_Impl_Frodo_Sample_frodo_sample_matrix1344(x231, x234, x237, x238);
   return Val_unit;
}
value _16_randombytes_(value x241, value x240)
{
   uint32_t x242 = Uint32_val(x241);
   unsigned char* x245 = CTYPES_PTR_OF_OCAML_BYTES(x240);
   randombytes_(x242, x245);
   return Val_unit;
}
value _17_Hacl_Impl_Frodo_Pack_frodo_pack(value x251, value x250, value x249,
                                          value x248, value x247)
{
   uint32_t x252 = Uint32_val(x251);
   uint32_t x255 = Uint32_val(x250);
   uint32_t x258 = Uint32_val(x249);
   uint16_t* x261 = CTYPES_ADDR_OF_FATPTR(x248);
   unsigned char* x262 = CTYPES_PTR_OF_OCAML_BYTES(x247);
   Hacl_Impl_Frodo_Pack_frodo_pack(x252, x255, x258, x261, x262);
   return Val_unit;
}
value _18_Hacl_Impl_Frodo_Pack_frodo_unpack(value x268, value x267,
                                            value x266, value x265,
                                            value x264)
{
   uint32_t x269 = Uint32_val(x268);
   uint32_t x272 = Uint32_val(x267);
   uint32_t x275 = Uint32_val(x266);
   unsigned char* x278 = CTYPES_PTR_OF_OCAML_BYTES(x265);
   uint16_t* x279 = CTYPES_ADDR_OF_FATPTR(x264);
   Hacl_Impl_Frodo_Pack_frodo_unpack(x269, x272, x275, x278, x279);
   return Val_unit;
}
value _19_Hacl_Impl_Frodo_Encode_frodo_key_encode(value x285, value x284,
                                                  value x283, value x282,
                                                  value x281)
{
   uint32_t x286 = Uint32_val(x285);
   uint32_t x289 = Uint32_val(x284);
   uint32_t x292 = Uint32_val(x283);
   unsigned char* x295 = CTYPES_PTR_OF_OCAML_BYTES(x282);
   uint16_t* x296 = CTYPES_ADDR_OF_FATPTR(x281);
   Hacl_Impl_Frodo_Encode_frodo_key_encode(x286, x289, x292, x295, x296);
   return Val_unit;
}
value _20_Hacl_Impl_Frodo_Encode_frodo_key_decode(value x302, value x301,
                                                  value x300, value x299,
                                                  value x298)
{
   uint32_t x303 = Uint32_val(x302);
   uint32_t x306 = Uint32_val(x301);
   uint32_t x309 = Uint32_val(x300);
   uint16_t* x312 = CTYPES_ADDR_OF_FATPTR(x299);
   unsigned char* x313 = CTYPES_PTR_OF_OCAML_BYTES(x298);
   Hacl_Impl_Frodo_Encode_frodo_key_decode(x303, x306, x309, x312, x313);
   return Val_unit;
}
