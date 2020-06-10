
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
value _9_EverCrypt_HKDF_expand(value x167, value x166, value x165,
                               value x164, value x163, value x162,
                               value x161)
{
   uint8_t x168 = Uint8_val(x167);
   unsigned char* x171 = CTYPES_PTR_OF_OCAML_BYTES(x166);
   unsigned char* x172 = CTYPES_PTR_OF_OCAML_BYTES(x165);
   uint32_t x173 = Uint32_val(x164);
   unsigned char* x176 = CTYPES_PTR_OF_OCAML_BYTES(x163);
   uint32_t x177 = Uint32_val(x162);
   uint32_t x180 = Uint32_val(x161);
   EverCrypt_HKDF_expand(x168, x171, x172, x173, x176, x177, x180);
   return Val_unit;
}
value _9_EverCrypt_HKDF_expand_byte7(value* argv, int argc)
{
   value x184 = argv[6];
   value x185 = argv[5];
   value x186 = argv[4];
   value x187 = argv[3];
   value x188 = argv[2];
   value x189 = argv[1];
   value x190 = argv[0];
   return _9_EverCrypt_HKDF_expand(x190, x189, x188, x187, x186, x185, x184);
}
value _10_EverCrypt_HKDF_extract(value x196, value x195, value x194,
                                 value x193, value x192, value x191)
{
   uint8_t x197 = Uint8_val(x196);
   unsigned char* x200 = CTYPES_PTR_OF_OCAML_BYTES(x195);
   unsigned char* x201 = CTYPES_PTR_OF_OCAML_BYTES(x194);
   uint32_t x202 = Uint32_val(x193);
   unsigned char* x205 = CTYPES_PTR_OF_OCAML_BYTES(x192);
   uint32_t x206 = Uint32_val(x191);
   EverCrypt_HKDF_extract(x197, x200, x201, x202, x205, x206);
   return Val_unit;
}
value _10_EverCrypt_HKDF_extract_byte6(value* argv, int argc)
{
   value x210 = argv[5];
   value x211 = argv[4];
   value x212 = argv[3];
   value x213 = argv[2];
   value x214 = argv[1];
   value x215 = argv[0];
   return _10_EverCrypt_HKDF_extract(x215, x214, x213, x212, x211, x210);
}
value _11_EverCrypt_HKDF_hkdf_expand(value x222, value x221, value x220,
                                     value x219, value x218, value x217,
                                     value x216)
{
   uint8_t x223 = Uint8_val(x222);
   unsigned char* x226 = CTYPES_PTR_OF_OCAML_BYTES(x221);
   unsigned char* x227 = CTYPES_PTR_OF_OCAML_BYTES(x220);
   uint32_t x228 = Uint32_val(x219);
   unsigned char* x231 = CTYPES_PTR_OF_OCAML_BYTES(x218);
   uint32_t x232 = Uint32_val(x217);
   uint32_t x235 = Uint32_val(x216);
   EverCrypt_HKDF_hkdf_expand(x223, x226, x227, x228, x231, x232, x235);
   return Val_unit;
}
value _11_EverCrypt_HKDF_hkdf_expand_byte7(value* argv, int argc)
{
   value x239 = argv[6];
   value x240 = argv[5];
   value x241 = argv[4];
   value x242 = argv[3];
   value x243 = argv[2];
   value x244 = argv[1];
   value x245 = argv[0];
   return
     _11_EverCrypt_HKDF_hkdf_expand(x245, x244, x243, x242, x241, x240, x239);
}
value _12_EverCrypt_HKDF_hkdf_extract(value x251, value x250, value x249,
                                      value x248, value x247, value x246)
{
   uint8_t x252 = Uint8_val(x251);
   unsigned char* x255 = CTYPES_PTR_OF_OCAML_BYTES(x250);
   unsigned char* x256 = CTYPES_PTR_OF_OCAML_BYTES(x249);
   uint32_t x257 = Uint32_val(x248);
   unsigned char* x260 = CTYPES_PTR_OF_OCAML_BYTES(x247);
   uint32_t x261 = Uint32_val(x246);
   EverCrypt_HKDF_hkdf_extract(x252, x255, x256, x257, x260, x261);
   return Val_unit;
}
value _12_EverCrypt_HKDF_hkdf_extract_byte6(value* argv, int argc)
{
   value x265 = argv[5];
   value x266 = argv[4];
   value x267 = argv[3];
   value x268 = argv[2];
   value x269 = argv[1];
   value x270 = argv[0];
   return _12_EverCrypt_HKDF_hkdf_extract(x270, x269, x268, x267, x266, x265);
}
