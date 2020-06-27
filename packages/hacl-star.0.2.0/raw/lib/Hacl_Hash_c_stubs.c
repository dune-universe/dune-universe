
#include "Hacl_Hash.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Hash_MD5_legacy_update_multi(value x3, value x2, value x1)
{
   uint32_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   unsigned char* x5 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x6 = Uint32_val(x1);
   Hacl_Hash_MD5_legacy_update_multi(x4, x5, x6);
   return Val_unit;
}
value _2_Hacl_Hash_MD5_legacy_update_last(value x13, value x12, value x11,
                                          value x10)
{
   uint32_t* x14 = CTYPES_ADDR_OF_FATPTR(x13);
   uint64_t x15 = Uint64_val(x12);
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x11);
   uint32_t x19 = Uint32_val(x10);
   Hacl_Hash_MD5_legacy_update_last(x14, x15, x18, x19);
   return Val_unit;
}
value _3_Hacl_Hash_MD5_legacy_hash(value x25, value x24, value x23)
{
   unsigned char* x26 = CTYPES_PTR_OF_OCAML_BYTES(x25);
   uint32_t x27 = Uint32_val(x24);
   unsigned char* x30 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   Hacl_Hash_MD5_legacy_hash(x26, x27, x30);
   return Val_unit;
}
value _4_Hacl_Hash_Core_MD5_legacy_init(value x32)
{
   uint32_t* x33 = CTYPES_ADDR_OF_FATPTR(x32);
   Hacl_Hash_Core_MD5_legacy_init(x33);
   return Val_unit;
}
value _5_Hacl_Hash_Core_MD5_legacy_update(value x36, value x35)
{
   uint32_t* x37 = CTYPES_ADDR_OF_FATPTR(x36);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   Hacl_Hash_Core_MD5_legacy_update(x37, x38);
   return Val_unit;
}
value _6_Hacl_Hash_Core_MD5_legacy_pad(value x41, value x40)
{
   uint64_t x42 = Uint64_val(x41);
   unsigned char* x45 = CTYPES_PTR_OF_OCAML_BYTES(x40);
   Hacl_Hash_Core_MD5_legacy_pad(x42, x45);
   return Val_unit;
}
value _7_Hacl_Hash_Core_MD5_legacy_finish(value x48, value x47)
{
   uint32_t* x49 = CTYPES_ADDR_OF_FATPTR(x48);
   unsigned char* x50 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   Hacl_Hash_Core_MD5_legacy_finish(x49, x50);
   return Val_unit;
}
value _8_Hacl_Hash_SHA1_legacy_update_multi(value x54, value x53, value x52)
{
   uint32_t* x55 = CTYPES_ADDR_OF_FATPTR(x54);
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x53);
   uint32_t x57 = Uint32_val(x52);
   Hacl_Hash_SHA1_legacy_update_multi(x55, x56, x57);
   return Val_unit;
}
value _9_Hacl_Hash_SHA1_legacy_update_last(value x64, value x63, value x62,
                                           value x61)
{
   uint32_t* x65 = CTYPES_ADDR_OF_FATPTR(x64);
   uint64_t x66 = Uint64_val(x63);
   unsigned char* x69 = CTYPES_PTR_OF_OCAML_BYTES(x62);
   uint32_t x70 = Uint32_val(x61);
   Hacl_Hash_SHA1_legacy_update_last(x65, x66, x69, x70);
   return Val_unit;
}
value _10_Hacl_Hash_SHA1_legacy_hash(value x76, value x75, value x74)
{
   unsigned char* x77 = CTYPES_PTR_OF_OCAML_BYTES(x76);
   uint32_t x78 = Uint32_val(x75);
   unsigned char* x81 = CTYPES_PTR_OF_OCAML_BYTES(x74);
   Hacl_Hash_SHA1_legacy_hash(x77, x78, x81);
   return Val_unit;
}
value _11_Hacl_Hash_Core_SHA1_legacy_init(value x83)
{
   uint32_t* x84 = CTYPES_ADDR_OF_FATPTR(x83);
   Hacl_Hash_Core_SHA1_legacy_init(x84);
   return Val_unit;
}
value _12_Hacl_Hash_Core_SHA1_legacy_update(value x87, value x86)
{
   uint32_t* x88 = CTYPES_ADDR_OF_FATPTR(x87);
   unsigned char* x89 = CTYPES_PTR_OF_OCAML_BYTES(x86);
   Hacl_Hash_Core_SHA1_legacy_update(x88, x89);
   return Val_unit;
}
value _13_Hacl_Hash_Core_SHA1_legacy_pad(value x92, value x91)
{
   uint64_t x93 = Uint64_val(x92);
   unsigned char* x96 = CTYPES_PTR_OF_OCAML_BYTES(x91);
   Hacl_Hash_Core_SHA1_legacy_pad(x93, x96);
   return Val_unit;
}
value _14_Hacl_Hash_Core_SHA1_legacy_finish(value x99, value x98)
{
   uint32_t* x100 = CTYPES_ADDR_OF_FATPTR(x99);
   unsigned char* x101 = CTYPES_PTR_OF_OCAML_BYTES(x98);
   Hacl_Hash_Core_SHA1_legacy_finish(x100, x101);
   return Val_unit;
}
value _15_Hacl_Hash_SHA2_update_multi_224(value x105, value x104, value x103)
{
   uint32_t* x106 = CTYPES_ADDR_OF_FATPTR(x105);
   unsigned char* x107 = CTYPES_PTR_OF_OCAML_BYTES(x104);
   uint32_t x108 = Uint32_val(x103);
   Hacl_Hash_SHA2_update_multi_224(x106, x107, x108);
   return Val_unit;
}
value _16_Hacl_Hash_SHA2_update_multi_256(value x114, value x113, value x112)
{
   uint32_t* x115 = CTYPES_ADDR_OF_FATPTR(x114);
   unsigned char* x116 = CTYPES_PTR_OF_OCAML_BYTES(x113);
   uint32_t x117 = Uint32_val(x112);
   Hacl_Hash_SHA2_update_multi_256(x115, x116, x117);
   return Val_unit;
}
value _17_Hacl_Hash_SHA2_update_multi_384(value x123, value x122, value x121)
{
   uint64_t* x124 = CTYPES_ADDR_OF_FATPTR(x123);
   unsigned char* x125 = CTYPES_PTR_OF_OCAML_BYTES(x122);
   uint32_t x126 = Uint32_val(x121);
   Hacl_Hash_SHA2_update_multi_384(x124, x125, x126);
   return Val_unit;
}
value _18_Hacl_Hash_SHA2_update_multi_512(value x132, value x131, value x130)
{
   uint64_t* x133 = CTYPES_ADDR_OF_FATPTR(x132);
   unsigned char* x134 = CTYPES_PTR_OF_OCAML_BYTES(x131);
   uint32_t x135 = Uint32_val(x130);
   Hacl_Hash_SHA2_update_multi_512(x133, x134, x135);
   return Val_unit;
}
value _19_Hacl_Hash_SHA2_update_last_224(value x142, value x141, value x140,
                                         value x139)
{
   uint32_t* x143 = CTYPES_ADDR_OF_FATPTR(x142);
   uint64_t x144 = Uint64_val(x141);
   unsigned char* x147 = CTYPES_PTR_OF_OCAML_BYTES(x140);
   uint32_t x148 = Uint32_val(x139);
   Hacl_Hash_SHA2_update_last_224(x143, x144, x147, x148);
   return Val_unit;
}
value _20_Hacl_Hash_SHA2_update_last_256(value x155, value x154, value x153,
                                         value x152)
{
   uint32_t* x156 = CTYPES_ADDR_OF_FATPTR(x155);
   uint64_t x157 = Uint64_val(x154);
   unsigned char* x160 = CTYPES_PTR_OF_OCAML_BYTES(x153);
   uint32_t x161 = Uint32_val(x152);
   Hacl_Hash_SHA2_update_last_256(x156, x157, x160, x161);
   return Val_unit;
}
value _21_Hacl_Hash_SHA2_hash_224(value x167, value x166, value x165)
{
   unsigned char* x168 = CTYPES_PTR_OF_OCAML_BYTES(x167);
   uint32_t x169 = Uint32_val(x166);
   unsigned char* x172 = CTYPES_PTR_OF_OCAML_BYTES(x165);
   Hacl_Hash_SHA2_hash_224(x168, x169, x172);
   return Val_unit;
}
value _22_Hacl_Hash_SHA2_hash_256(value x176, value x175, value x174)
{
   unsigned char* x177 = CTYPES_PTR_OF_OCAML_BYTES(x176);
   uint32_t x178 = Uint32_val(x175);
   unsigned char* x181 = CTYPES_PTR_OF_OCAML_BYTES(x174);
   Hacl_Hash_SHA2_hash_256(x177, x178, x181);
   return Val_unit;
}
value _23_Hacl_Hash_SHA2_hash_384(value x185, value x184, value x183)
{
   unsigned char* x186 = CTYPES_PTR_OF_OCAML_BYTES(x185);
   uint32_t x187 = Uint32_val(x184);
   unsigned char* x190 = CTYPES_PTR_OF_OCAML_BYTES(x183);
   Hacl_Hash_SHA2_hash_384(x186, x187, x190);
   return Val_unit;
}
value _24_Hacl_Hash_SHA2_hash_512(value x194, value x193, value x192)
{
   unsigned char* x195 = CTYPES_PTR_OF_OCAML_BYTES(x194);
   uint32_t x196 = Uint32_val(x193);
   unsigned char* x199 = CTYPES_PTR_OF_OCAML_BYTES(x192);
   Hacl_Hash_SHA2_hash_512(x195, x196, x199);
   return Val_unit;
}
value _25_Hacl_Hash_Core_SHA2_init_224(value x201)
{
   uint32_t* x202 = CTYPES_ADDR_OF_FATPTR(x201);
   Hacl_Hash_Core_SHA2_init_224(x202);
   return Val_unit;
}
value _26_Hacl_Hash_Core_SHA2_init_256(value x204)
{
   uint32_t* x205 = CTYPES_ADDR_OF_FATPTR(x204);
   Hacl_Hash_Core_SHA2_init_256(x205);
   return Val_unit;
}
value _27_Hacl_Hash_Core_SHA2_init_384(value x207)
{
   uint64_t* x208 = CTYPES_ADDR_OF_FATPTR(x207);
   Hacl_Hash_Core_SHA2_init_384(x208);
   return Val_unit;
}
value _28_Hacl_Hash_Core_SHA2_init_512(value x210)
{
   uint64_t* x211 = CTYPES_ADDR_OF_FATPTR(x210);
   Hacl_Hash_Core_SHA2_init_512(x211);
   return Val_unit;
}
value _29_Hacl_Hash_Core_SHA2_update_224(value x214, value x213)
{
   uint32_t* x215 = CTYPES_ADDR_OF_FATPTR(x214);
   unsigned char* x216 = CTYPES_PTR_OF_OCAML_BYTES(x213);
   Hacl_Hash_Core_SHA2_update_224(x215, x216);
   return Val_unit;
}
value _30_Hacl_Hash_Core_SHA2_update_256(value x219, value x218)
{
   uint32_t* x220 = CTYPES_ADDR_OF_FATPTR(x219);
   unsigned char* x221 = CTYPES_PTR_OF_OCAML_BYTES(x218);
   Hacl_Hash_Core_SHA2_update_256(x220, x221);
   return Val_unit;
}
value _31_Hacl_Hash_Core_SHA2_update_384(value x224, value x223)
{
   uint64_t* x225 = CTYPES_ADDR_OF_FATPTR(x224);
   unsigned char* x226 = CTYPES_PTR_OF_OCAML_BYTES(x223);
   Hacl_Hash_Core_SHA2_update_384(x225, x226);
   return Val_unit;
}
value _32_Hacl_Hash_Core_SHA2_update_512(value x229, value x228)
{
   uint64_t* x230 = CTYPES_ADDR_OF_FATPTR(x229);
   unsigned char* x231 = CTYPES_PTR_OF_OCAML_BYTES(x228);
   Hacl_Hash_Core_SHA2_update_512(x230, x231);
   return Val_unit;
}
value _33_Hacl_Hash_Core_SHA2_pad_224(value x234, value x233)
{
   uint64_t x235 = Uint64_val(x234);
   unsigned char* x238 = CTYPES_PTR_OF_OCAML_BYTES(x233);
   Hacl_Hash_Core_SHA2_pad_224(x235, x238);
   return Val_unit;
}
value _34_Hacl_Hash_Core_SHA2_pad_256(value x241, value x240)
{
   uint64_t x242 = Uint64_val(x241);
   unsigned char* x245 = CTYPES_PTR_OF_OCAML_BYTES(x240);
   Hacl_Hash_Core_SHA2_pad_256(x242, x245);
   return Val_unit;
}
value _35_Hacl_Hash_Core_SHA2_finish_224(value x248, value x247)
{
   uint32_t* x249 = CTYPES_ADDR_OF_FATPTR(x248);
   unsigned char* x250 = CTYPES_PTR_OF_OCAML_BYTES(x247);
   Hacl_Hash_Core_SHA2_finish_224(x249, x250);
   return Val_unit;
}
value _36_Hacl_Hash_Core_SHA2_finish_256(value x253, value x252)
{
   uint32_t* x254 = CTYPES_ADDR_OF_FATPTR(x253);
   unsigned char* x255 = CTYPES_PTR_OF_OCAML_BYTES(x252);
   Hacl_Hash_Core_SHA2_finish_256(x254, x255);
   return Val_unit;
}
value _37_Hacl_Hash_Core_SHA2_finish_384(value x258, value x257)
{
   uint64_t* x259 = CTYPES_ADDR_OF_FATPTR(x258);
   unsigned char* x260 = CTYPES_PTR_OF_OCAML_BYTES(x257);
   Hacl_Hash_Core_SHA2_finish_384(x259, x260);
   return Val_unit;
}
value _38_Hacl_Hash_Core_SHA2_finish_512(value x263, value x262)
{
   uint64_t* x264 = CTYPES_ADDR_OF_FATPTR(x263);
   unsigned char* x265 = CTYPES_PTR_OF_OCAML_BYTES(x262);
   Hacl_Hash_Core_SHA2_finish_512(x264, x265);
   return Val_unit;
}
value _39_Hacl_Hash_Definitions_word_len(value x267)
{
   uint8_t x268 = Uint8_val(x267);
   uint32_t x271 = Hacl_Hash_Definitions_word_len(x268);
   return integers_copy_uint32(x271);
}
value _40_Hacl_Hash_Definitions_block_len(value x272)
{
   uint8_t x273 = Uint8_val(x272);
   uint32_t x276 = Hacl_Hash_Definitions_block_len(x273);
   return integers_copy_uint32(x276);
}
value _41_Hacl_Hash_Definitions_hash_word_len(value x277)
{
   uint8_t x278 = Uint8_val(x277);
   uint32_t x281 = Hacl_Hash_Definitions_hash_word_len(x278);
   return integers_copy_uint32(x281);
}
value _42_Hacl_Hash_Definitions_hash_len(value x282)
{
   uint8_t x283 = Uint8_val(x282);
   uint32_t x286 = Hacl_Hash_Definitions_hash_len(x283);
   return integers_copy_uint32(x286);
}
