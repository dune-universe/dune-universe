
#include "EverCrypt_HKDF.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_HKDF_expand_sha1(value x6, value x5, value x4, value x3,
                                    value x2, value x1)
{
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x9 = Uint32_val(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x13 = Uint32_val(x2);
   uint32_t x16 = Uint32_val(x1);
   EverCrypt_HKDF_expand_sha1(x7, x8, x9, x12, x13, x16);
   return Val_unit;
}
value _1_EverCrypt_HKDF_expand_sha1_byte6(value* argv, int argc)
{
   value x20 = argv[5];
   value x21 = argv[4];
   value x22 = argv[3];
   value x23 = argv[2];
   value x24 = argv[1];
   value x25 = argv[0];
   return _1_EverCrypt_HKDF_expand_sha1(x25, x24, x23, x22, x21, x20);
}
value _2_EverCrypt_HKDF_extract_sha1(value x30, value x29, value x28,
                                     value x27, value x26)
{
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x30);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   uint32_t x33 = Uint32_val(x28);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint32_t x37 = Uint32_val(x26);
   EverCrypt_HKDF_extract_sha1(x31, x32, x33, x36, x37);
   return Val_unit;
}
value _3_EverCrypt_HKDF_expand_sha2_256(value x46, value x45, value x44,
                                        value x43, value x42, value x41)
{
   unsigned char* x47 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   unsigned char* x48 = CTYPES_PTR_OF_OCAML_BYTES(x45);
   uint32_t x49 = Uint32_val(x44);
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x43);
   uint32_t x53 = Uint32_val(x42);
   uint32_t x56 = Uint32_val(x41);
   EverCrypt_HKDF_expand_sha2_256(x47, x48, x49, x52, x53, x56);
   return Val_unit;
}
value _3_EverCrypt_HKDF_expand_sha2_256_byte6(value* argv, int argc)
{
   value x60 = argv[5];
   value x61 = argv[4];
   value x62 = argv[3];
   value x63 = argv[2];
   value x64 = argv[1];
   value x65 = argv[0];
   return _3_EverCrypt_HKDF_expand_sha2_256(x65, x64, x63, x62, x61, x60);
}
value _4_EverCrypt_HKDF_extract_sha2_256(value x70, value x69, value x68,
                                         value x67, value x66)
{
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x70);
   unsigned char* x72 = CTYPES_PTR_OF_OCAML_BYTES(x69);
   uint32_t x73 = Uint32_val(x68);
   unsigned char* x76 = CTYPES_PTR_OF_OCAML_BYTES(x67);
   uint32_t x77 = Uint32_val(x66);
   EverCrypt_HKDF_extract_sha2_256(x71, x72, x73, x76, x77);
   return Val_unit;
}
value _5_EverCrypt_HKDF_expand_sha2_384(value x86, value x85, value x84,
                                        value x83, value x82, value x81)
{
   unsigned char* x87 = CTYPES_PTR_OF_OCAML_BYTES(x86);
   unsigned char* x88 = CTYPES_PTR_OF_OCAML_BYTES(x85);
   uint32_t x89 = Uint32_val(x84);
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x83);
   uint32_t x93 = Uint32_val(x82);
   uint32_t x96 = Uint32_val(x81);
   EverCrypt_HKDF_expand_sha2_384(x87, x88, x89, x92, x93, x96);
   return Val_unit;
}
value _5_EverCrypt_HKDF_expand_sha2_384_byte6(value* argv, int argc)
{
   value x100 = argv[5];
   value x101 = argv[4];
   value x102 = argv[3];
   value x103 = argv[2];
   value x104 = argv[1];
   value x105 = argv[0];
   return
     _5_EverCrypt_HKDF_expand_sha2_384(x105, x104, x103, x102, x101, x100);
}
value _6_EverCrypt_HKDF_extract_sha2_384(value x110, value x109, value x108,
                                         value x107, value x106)
{
   unsigned char* x111 = CTYPES_PTR_OF_OCAML_BYTES(x110);
   unsigned char* x112 = CTYPES_PTR_OF_OCAML_BYTES(x109);
   uint32_t x113 = Uint32_val(x108);
   unsigned char* x116 = CTYPES_PTR_OF_OCAML_BYTES(x107);
   uint32_t x117 = Uint32_val(x106);
   EverCrypt_HKDF_extract_sha2_384(x111, x112, x113, x116, x117);
   return Val_unit;
}
value _7_EverCrypt_HKDF_expand_sha2_512(value x126, value x125, value x124,
                                        value x123, value x122, value x121)
{
   unsigned char* x127 = CTYPES_PTR_OF_OCAML_BYTES(x126);
   unsigned char* x128 = CTYPES_PTR_OF_OCAML_BYTES(x125);
   uint32_t x129 = Uint32_val(x124);
   unsigned char* x132 = CTYPES_PTR_OF_OCAML_BYTES(x123);
   uint32_t x133 = Uint32_val(x122);
   uint32_t x136 = Uint32_val(x121);
   EverCrypt_HKDF_expand_sha2_512(x127, x128, x129, x132, x133, x136);
   return Val_unit;
}
value _7_EverCrypt_HKDF_expand_sha2_512_byte6(value* argv, int argc)
{
   value x140 = argv[5];
   value x141 = argv[4];
   value x142 = argv[3];
   value x143 = argv[2];
   value x144 = argv[1];
   value x145 = argv[0];
   return
     _7_EverCrypt_HKDF_expand_sha2_512(x145, x144, x143, x142, x141, x140);
}
value _8_EverCrypt_HKDF_extract_sha2_512(value x150, value x149, value x148,
                                         value x147, value x146)
{
   unsigned char* x151 = CTYPES_PTR_OF_OCAML_BYTES(x150);
   unsigned char* x152 = CTYPES_PTR_OF_OCAML_BYTES(x149);
   uint32_t x153 = Uint32_val(x148);
   unsigned char* x156 = CTYPES_PTR_OF_OCAML_BYTES(x147);
   uint32_t x157 = Uint32_val(x146);
   EverCrypt_HKDF_extract_sha2_512(x151, x152, x153, x156, x157);
   return Val_unit;
}
value _9_EverCrypt_HKDF_expand_blake2s(value x166, value x165, value x164,
                                       value x163, value x162, value x161)
{
   unsigned char* x167 = CTYPES_PTR_OF_OCAML_BYTES(x166);
   unsigned char* x168 = CTYPES_PTR_OF_OCAML_BYTES(x165);
   uint32_t x169 = Uint32_val(x164);
   unsigned char* x172 = CTYPES_PTR_OF_OCAML_BYTES(x163);
   uint32_t x173 = Uint32_val(x162);
   uint32_t x176 = Uint32_val(x161);
   EverCrypt_HKDF_expand_blake2s(x167, x168, x169, x172, x173, x176);
   return Val_unit;
}
value _9_EverCrypt_HKDF_expand_blake2s_byte6(value* argv, int argc)
{
   value x180 = argv[5];
   value x181 = argv[4];
   value x182 = argv[3];
   value x183 = argv[2];
   value x184 = argv[1];
   value x185 = argv[0];
   return
     _9_EverCrypt_HKDF_expand_blake2s(x185, x184, x183, x182, x181, x180);
}
value _10_EverCrypt_HKDF_extract_blake2s(value x190, value x189, value x188,
                                         value x187, value x186)
{
   unsigned char* x191 = CTYPES_PTR_OF_OCAML_BYTES(x190);
   unsigned char* x192 = CTYPES_PTR_OF_OCAML_BYTES(x189);
   uint32_t x193 = Uint32_val(x188);
   unsigned char* x196 = CTYPES_PTR_OF_OCAML_BYTES(x187);
   uint32_t x197 = Uint32_val(x186);
   EverCrypt_HKDF_extract_blake2s(x191, x192, x193, x196, x197);
   return Val_unit;
}
value _11_EverCrypt_HKDF_expand_blake2b(value x206, value x205, value x204,
                                        value x203, value x202, value x201)
{
   unsigned char* x207 = CTYPES_PTR_OF_OCAML_BYTES(x206);
   unsigned char* x208 = CTYPES_PTR_OF_OCAML_BYTES(x205);
   uint32_t x209 = Uint32_val(x204);
   unsigned char* x212 = CTYPES_PTR_OF_OCAML_BYTES(x203);
   uint32_t x213 = Uint32_val(x202);
   uint32_t x216 = Uint32_val(x201);
   EverCrypt_HKDF_expand_blake2b(x207, x208, x209, x212, x213, x216);
   return Val_unit;
}
value _11_EverCrypt_HKDF_expand_blake2b_byte6(value* argv, int argc)
{
   value x220 = argv[5];
   value x221 = argv[4];
   value x222 = argv[3];
   value x223 = argv[2];
   value x224 = argv[1];
   value x225 = argv[0];
   return
     _11_EverCrypt_HKDF_expand_blake2b(x225, x224, x223, x222, x221, x220);
}
value _12_EverCrypt_HKDF_extract_blake2b(value x230, value x229, value x228,
                                         value x227, value x226)
{
   unsigned char* x231 = CTYPES_PTR_OF_OCAML_BYTES(x230);
   unsigned char* x232 = CTYPES_PTR_OF_OCAML_BYTES(x229);
   uint32_t x233 = Uint32_val(x228);
   unsigned char* x236 = CTYPES_PTR_OF_OCAML_BYTES(x227);
   uint32_t x237 = Uint32_val(x226);
   EverCrypt_HKDF_extract_blake2b(x231, x232, x233, x236, x237);
   return Val_unit;
}
value _13_EverCrypt_HKDF_expand(value x247, value x246, value x245,
                                value x244, value x243, value x242,
                                value x241)
{
   uint8_t x248 = Uint8_val(x247);
   unsigned char* x251 = CTYPES_PTR_OF_OCAML_BYTES(x246);
   unsigned char* x252 = CTYPES_PTR_OF_OCAML_BYTES(x245);
   uint32_t x253 = Uint32_val(x244);
   unsigned char* x256 = CTYPES_PTR_OF_OCAML_BYTES(x243);
   uint32_t x257 = Uint32_val(x242);
   uint32_t x260 = Uint32_val(x241);
   EverCrypt_HKDF_expand(x248, x251, x252, x253, x256, x257, x260);
   return Val_unit;
}
value _13_EverCrypt_HKDF_expand_byte7(value* argv, int argc)
{
   value x264 = argv[6];
   value x265 = argv[5];
   value x266 = argv[4];
   value x267 = argv[3];
   value x268 = argv[2];
   value x269 = argv[1];
   value x270 = argv[0];
   return _13_EverCrypt_HKDF_expand(x270, x269, x268, x267, x266, x265, x264);
}
value _14_EverCrypt_HKDF_extract(value x276, value x275, value x274,
                                 value x273, value x272, value x271)
{
   uint8_t x277 = Uint8_val(x276);
   unsigned char* x280 = CTYPES_PTR_OF_OCAML_BYTES(x275);
   unsigned char* x281 = CTYPES_PTR_OF_OCAML_BYTES(x274);
   uint32_t x282 = Uint32_val(x273);
   unsigned char* x285 = CTYPES_PTR_OF_OCAML_BYTES(x272);
   uint32_t x286 = Uint32_val(x271);
   EverCrypt_HKDF_extract(x277, x280, x281, x282, x285, x286);
   return Val_unit;
}
value _14_EverCrypt_HKDF_extract_byte6(value* argv, int argc)
{
   value x290 = argv[5];
   value x291 = argv[4];
   value x292 = argv[3];
   value x293 = argv[2];
   value x294 = argv[1];
   value x295 = argv[0];
   return _14_EverCrypt_HKDF_extract(x295, x294, x293, x292, x291, x290);
}
value _15_EverCrypt_HKDF_hkdf_expand(value x302, value x301, value x300,
                                     value x299, value x298, value x297,
                                     value x296)
{
   uint8_t x303 = Uint8_val(x302);
   unsigned char* x306 = CTYPES_PTR_OF_OCAML_BYTES(x301);
   unsigned char* x307 = CTYPES_PTR_OF_OCAML_BYTES(x300);
   uint32_t x308 = Uint32_val(x299);
   unsigned char* x311 = CTYPES_PTR_OF_OCAML_BYTES(x298);
   uint32_t x312 = Uint32_val(x297);
   uint32_t x315 = Uint32_val(x296);
   EverCrypt_HKDF_hkdf_expand(x303, x306, x307, x308, x311, x312, x315);
   return Val_unit;
}
value _15_EverCrypt_HKDF_hkdf_expand_byte7(value* argv, int argc)
{
   value x319 = argv[6];
   value x320 = argv[5];
   value x321 = argv[4];
   value x322 = argv[3];
   value x323 = argv[2];
   value x324 = argv[1];
   value x325 = argv[0];
   return
     _15_EverCrypt_HKDF_hkdf_expand(x325, x324, x323, x322, x321, x320, x319);
}
value _16_EverCrypt_HKDF_hkdf_extract(value x331, value x330, value x329,
                                      value x328, value x327, value x326)
{
   uint8_t x332 = Uint8_val(x331);
   unsigned char* x335 = CTYPES_PTR_OF_OCAML_BYTES(x330);
   unsigned char* x336 = CTYPES_PTR_OF_OCAML_BYTES(x329);
   uint32_t x337 = Uint32_val(x328);
   unsigned char* x340 = CTYPES_PTR_OF_OCAML_BYTES(x327);
   uint32_t x341 = Uint32_val(x326);
   EverCrypt_HKDF_hkdf_extract(x332, x335, x336, x337, x340, x341);
   return Val_unit;
}
value _16_EverCrypt_HKDF_hkdf_extract_byte6(value* argv, int argc)
{
   value x345 = argv[5];
   value x346 = argv[4];
   value x347 = argv[3];
   value x348 = argv[2];
   value x349 = argv[1];
   value x350 = argv[0];
   return _16_EverCrypt_HKDF_hkdf_extract(x350, x349, x348, x347, x346, x345);
}
