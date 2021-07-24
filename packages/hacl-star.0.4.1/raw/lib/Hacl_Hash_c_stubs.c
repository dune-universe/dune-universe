
#include "Hacl_Hash.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Hash_Core_Blake2_update_blake2s_32(value x3, value x2,
                                                 value x1)
{
   uint32_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   uint64_t x5 = Uint64_val(x2);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   uint64_t x9 = Hacl_Hash_Core_Blake2_update_blake2s_32(x4, x5, x8);
   return integers_copy_uint64(x9);
}
value _2_Hacl_Hash_Core_Blake2_finish_blake2s_32(value x12, value x11,
                                                 value x10)
{
   uint32_t* x13 = CTYPES_ADDR_OF_FATPTR(x12);
   uint64_t x14 = Uint64_val(x11);
   unsigned char* x17 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   Hacl_Hash_Core_Blake2_finish_blake2s_32(x13, x14, x17);
   return Val_unit;
}
value _3_Hacl_Hash_Blake2_update_multi_blake2s_32(value x22, value x21,
                                                  value x20, value x19)
{
   uint32_t* x23 = CTYPES_ADDR_OF_FATPTR(x22);
   uint64_t x24 = Uint64_val(x21);
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   uint32_t x28 = Uint32_val(x19);
   uint64_t x31 =
   Hacl_Hash_Blake2_update_multi_blake2s_32(x23, x24, x27, x28);
   return integers_copy_uint64(x31);
}
value _4_Hacl_Hash_Blake2_update_last_blake2s_32(value x36, value x35,
                                                 value x34, value x33,
                                                 value x32)
{
   uint32_t* x37 = CTYPES_ADDR_OF_FATPTR(x36);
   uint64_t x38 = Uint64_val(x35);
   uint64_t x41 = Uint64_val(x34);
   unsigned char* x44 = CTYPES_PTR_OF_OCAML_BYTES(x33);
   uint32_t x45 = Uint32_val(x32);
   uint64_t x48 =
   Hacl_Hash_Blake2_update_last_blake2s_32(x37, x38, x41, x44, x45);
   return integers_copy_uint64(x48);
}
value _5_Hacl_Hash_Blake2_hash_blake2s_32(value x51, value x50, value x49)
{
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x51);
   uint32_t x53 = Uint32_val(x50);
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x49);
   Hacl_Hash_Blake2_hash_blake2s_32(x52, x53, x56);
   return Val_unit;
}
value _6_Hacl_Hash_Blake2_hash_blake2b_32(value x60, value x59, value x58)
{
   unsigned char* x61 = CTYPES_PTR_OF_OCAML_BYTES(x60);
   uint32_t x62 = Uint32_val(x59);
   unsigned char* x65 = CTYPES_PTR_OF_OCAML_BYTES(x58);
   Hacl_Hash_Blake2_hash_blake2b_32(x61, x62, x65);
   return Val_unit;
}
value _7_Hacl_Hash_MD5_legacy_update_multi(value x69, value x68, value x67)
{
   uint32_t* x70 = CTYPES_ADDR_OF_FATPTR(x69);
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x68);
   uint32_t x72 = Uint32_val(x67);
   Hacl_Hash_MD5_legacy_update_multi(x70, x71, x72);
   return Val_unit;
}
value _8_Hacl_Hash_MD5_legacy_update_last(value x79, value x78, value x77,
                                          value x76)
{
   uint32_t* x80 = CTYPES_ADDR_OF_FATPTR(x79);
   uint64_t x81 = Uint64_val(x78);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x77);
   uint32_t x85 = Uint32_val(x76);
   Hacl_Hash_MD5_legacy_update_last(x80, x81, x84, x85);
   return Val_unit;
}
value _9_Hacl_Hash_MD5_legacy_hash(value x91, value x90, value x89)
{
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x91);
   uint32_t x93 = Uint32_val(x90);
   unsigned char* x96 = CTYPES_PTR_OF_OCAML_BYTES(x89);
   Hacl_Hash_MD5_legacy_hash(x92, x93, x96);
   return Val_unit;
}
value _10_Hacl_Hash_Core_MD5_legacy_init(value x98)
{
   uint32_t* x99 = CTYPES_ADDR_OF_FATPTR(x98);
   Hacl_Hash_Core_MD5_legacy_init(x99);
   return Val_unit;
}
value _11_Hacl_Hash_Core_MD5_legacy_update(value x102, value x101)
{
   uint32_t* x103 = CTYPES_ADDR_OF_FATPTR(x102);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x101);
   Hacl_Hash_Core_MD5_legacy_update(x103, x104);
   return Val_unit;
}
value _12_Hacl_Hash_Core_MD5_legacy_pad(value x107, value x106)
{
   uint64_t x108 = Uint64_val(x107);
   unsigned char* x111 = CTYPES_PTR_OF_OCAML_BYTES(x106);
   Hacl_Hash_Core_MD5_legacy_pad(x108, x111);
   return Val_unit;
}
value _13_Hacl_Hash_Core_MD5_legacy_finish(value x114, value x113)
{
   uint32_t* x115 = CTYPES_ADDR_OF_FATPTR(x114);
   unsigned char* x116 = CTYPES_PTR_OF_OCAML_BYTES(x113);
   Hacl_Hash_Core_MD5_legacy_finish(x115, x116);
   return Val_unit;
}
value _14_Hacl_Hash_SHA1_legacy_update_multi(value x120, value x119,
                                             value x118)
{
   uint32_t* x121 = CTYPES_ADDR_OF_FATPTR(x120);
   unsigned char* x122 = CTYPES_PTR_OF_OCAML_BYTES(x119);
   uint32_t x123 = Uint32_val(x118);
   Hacl_Hash_SHA1_legacy_update_multi(x121, x122, x123);
   return Val_unit;
}
value _15_Hacl_Hash_SHA1_legacy_update_last(value x130, value x129,
                                            value x128, value x127)
{
   uint32_t* x131 = CTYPES_ADDR_OF_FATPTR(x130);
   uint64_t x132 = Uint64_val(x129);
   unsigned char* x135 = CTYPES_PTR_OF_OCAML_BYTES(x128);
   uint32_t x136 = Uint32_val(x127);
   Hacl_Hash_SHA1_legacy_update_last(x131, x132, x135, x136);
   return Val_unit;
}
value _16_Hacl_Hash_SHA1_legacy_hash(value x142, value x141, value x140)
{
   unsigned char* x143 = CTYPES_PTR_OF_OCAML_BYTES(x142);
   uint32_t x144 = Uint32_val(x141);
   unsigned char* x147 = CTYPES_PTR_OF_OCAML_BYTES(x140);
   Hacl_Hash_SHA1_legacy_hash(x143, x144, x147);
   return Val_unit;
}
value _17_Hacl_Hash_Core_SHA1_legacy_init(value x149)
{
   uint32_t* x150 = CTYPES_ADDR_OF_FATPTR(x149);
   Hacl_Hash_Core_SHA1_legacy_init(x150);
   return Val_unit;
}
value _18_Hacl_Hash_Core_SHA1_legacy_update(value x153, value x152)
{
   uint32_t* x154 = CTYPES_ADDR_OF_FATPTR(x153);
   unsigned char* x155 = CTYPES_PTR_OF_OCAML_BYTES(x152);
   Hacl_Hash_Core_SHA1_legacy_update(x154, x155);
   return Val_unit;
}
value _19_Hacl_Hash_Core_SHA1_legacy_pad(value x158, value x157)
{
   uint64_t x159 = Uint64_val(x158);
   unsigned char* x162 = CTYPES_PTR_OF_OCAML_BYTES(x157);
   Hacl_Hash_Core_SHA1_legacy_pad(x159, x162);
   return Val_unit;
}
value _20_Hacl_Hash_Core_SHA1_legacy_finish(value x165, value x164)
{
   uint32_t* x166 = CTYPES_ADDR_OF_FATPTR(x165);
   unsigned char* x167 = CTYPES_PTR_OF_OCAML_BYTES(x164);
   Hacl_Hash_Core_SHA1_legacy_finish(x166, x167);
   return Val_unit;
}
value _21_Hacl_Hash_SHA2_update_multi_224(value x171, value x170, value x169)
{
   uint32_t* x172 = CTYPES_ADDR_OF_FATPTR(x171);
   unsigned char* x173 = CTYPES_PTR_OF_OCAML_BYTES(x170);
   uint32_t x174 = Uint32_val(x169);
   Hacl_Hash_SHA2_update_multi_224(x172, x173, x174);
   return Val_unit;
}
value _22_Hacl_Hash_SHA2_update_multi_256(value x180, value x179, value x178)
{
   uint32_t* x181 = CTYPES_ADDR_OF_FATPTR(x180);
   unsigned char* x182 = CTYPES_PTR_OF_OCAML_BYTES(x179);
   uint32_t x183 = Uint32_val(x178);
   Hacl_Hash_SHA2_update_multi_256(x181, x182, x183);
   return Val_unit;
}
value _23_Hacl_Hash_SHA2_update_multi_384(value x189, value x188, value x187)
{
   uint64_t* x190 = CTYPES_ADDR_OF_FATPTR(x189);
   unsigned char* x191 = CTYPES_PTR_OF_OCAML_BYTES(x188);
   uint32_t x192 = Uint32_val(x187);
   Hacl_Hash_SHA2_update_multi_384(x190, x191, x192);
   return Val_unit;
}
value _24_Hacl_Hash_SHA2_update_multi_512(value x198, value x197, value x196)
{
   uint64_t* x199 = CTYPES_ADDR_OF_FATPTR(x198);
   unsigned char* x200 = CTYPES_PTR_OF_OCAML_BYTES(x197);
   uint32_t x201 = Uint32_val(x196);
   Hacl_Hash_SHA2_update_multi_512(x199, x200, x201);
   return Val_unit;
}
value _25_Hacl_Hash_SHA2_update_last_224(value x208, value x207, value x206,
                                         value x205)
{
   uint32_t* x209 = CTYPES_ADDR_OF_FATPTR(x208);
   uint64_t x210 = Uint64_val(x207);
   unsigned char* x213 = CTYPES_PTR_OF_OCAML_BYTES(x206);
   uint32_t x214 = Uint32_val(x205);
   Hacl_Hash_SHA2_update_last_224(x209, x210, x213, x214);
   return Val_unit;
}
value _26_Hacl_Hash_SHA2_update_last_256(value x221, value x220, value x219,
                                         value x218)
{
   uint32_t* x222 = CTYPES_ADDR_OF_FATPTR(x221);
   uint64_t x223 = Uint64_val(x220);
   unsigned char* x226 = CTYPES_PTR_OF_OCAML_BYTES(x219);
   uint32_t x227 = Uint32_val(x218);
   Hacl_Hash_SHA2_update_last_256(x222, x223, x226, x227);
   return Val_unit;
}
value _27_Hacl_Hash_SHA2_hash_224(value x233, value x232, value x231)
{
   unsigned char* x234 = CTYPES_PTR_OF_OCAML_BYTES(x233);
   uint32_t x235 = Uint32_val(x232);
   unsigned char* x238 = CTYPES_PTR_OF_OCAML_BYTES(x231);
   Hacl_Hash_SHA2_hash_224(x234, x235, x238);
   return Val_unit;
}
value _28_Hacl_Hash_SHA2_hash_256(value x242, value x241, value x240)
{
   unsigned char* x243 = CTYPES_PTR_OF_OCAML_BYTES(x242);
   uint32_t x244 = Uint32_val(x241);
   unsigned char* x247 = CTYPES_PTR_OF_OCAML_BYTES(x240);
   Hacl_Hash_SHA2_hash_256(x243, x244, x247);
   return Val_unit;
}
value _29_Hacl_Hash_SHA2_hash_384(value x251, value x250, value x249)
{
   unsigned char* x252 = CTYPES_PTR_OF_OCAML_BYTES(x251);
   uint32_t x253 = Uint32_val(x250);
   unsigned char* x256 = CTYPES_PTR_OF_OCAML_BYTES(x249);
   Hacl_Hash_SHA2_hash_384(x252, x253, x256);
   return Val_unit;
}
value _30_Hacl_Hash_SHA2_hash_512(value x260, value x259, value x258)
{
   unsigned char* x261 = CTYPES_PTR_OF_OCAML_BYTES(x260);
   uint32_t x262 = Uint32_val(x259);
   unsigned char* x265 = CTYPES_PTR_OF_OCAML_BYTES(x258);
   Hacl_Hash_SHA2_hash_512(x261, x262, x265);
   return Val_unit;
}
value _31_Hacl_Hash_Core_SHA2_init_224(value x267)
{
   uint32_t* x268 = CTYPES_ADDR_OF_FATPTR(x267);
   Hacl_Hash_Core_SHA2_init_224(x268);
   return Val_unit;
}
value _32_Hacl_Hash_Core_SHA2_init_256(value x270)
{
   uint32_t* x271 = CTYPES_ADDR_OF_FATPTR(x270);
   Hacl_Hash_Core_SHA2_init_256(x271);
   return Val_unit;
}
value _33_Hacl_Hash_Core_SHA2_init_384(value x273)
{
   uint64_t* x274 = CTYPES_ADDR_OF_FATPTR(x273);
   Hacl_Hash_Core_SHA2_init_384(x274);
   return Val_unit;
}
value _34_Hacl_Hash_Core_SHA2_init_512(value x276)
{
   uint64_t* x277 = CTYPES_ADDR_OF_FATPTR(x276);
   Hacl_Hash_Core_SHA2_init_512(x277);
   return Val_unit;
}
value _35_Hacl_Hash_Core_SHA2_update_224(value x280, value x279)
{
   uint32_t* x281 = CTYPES_ADDR_OF_FATPTR(x280);
   unsigned char* x282 = CTYPES_PTR_OF_OCAML_BYTES(x279);
   Hacl_Hash_Core_SHA2_update_224(x281, x282);
   return Val_unit;
}
value _36_Hacl_Hash_Core_SHA2_update_256(value x285, value x284)
{
   uint32_t* x286 = CTYPES_ADDR_OF_FATPTR(x285);
   unsigned char* x287 = CTYPES_PTR_OF_OCAML_BYTES(x284);
   Hacl_Hash_Core_SHA2_update_256(x286, x287);
   return Val_unit;
}
value _37_Hacl_Hash_Core_SHA2_update_384(value x290, value x289)
{
   uint64_t* x291 = CTYPES_ADDR_OF_FATPTR(x290);
   unsigned char* x292 = CTYPES_PTR_OF_OCAML_BYTES(x289);
   Hacl_Hash_Core_SHA2_update_384(x291, x292);
   return Val_unit;
}
value _38_Hacl_Hash_Core_SHA2_update_512(value x295, value x294)
{
   uint64_t* x296 = CTYPES_ADDR_OF_FATPTR(x295);
   unsigned char* x297 = CTYPES_PTR_OF_OCAML_BYTES(x294);
   Hacl_Hash_Core_SHA2_update_512(x296, x297);
   return Val_unit;
}
value _39_Hacl_Hash_Core_SHA2_pad_224(value x300, value x299)
{
   uint64_t x301 = Uint64_val(x300);
   unsigned char* x304 = CTYPES_PTR_OF_OCAML_BYTES(x299);
   Hacl_Hash_Core_SHA2_pad_224(x301, x304);
   return Val_unit;
}
value _40_Hacl_Hash_Core_SHA2_pad_256(value x307, value x306)
{
   uint64_t x308 = Uint64_val(x307);
   unsigned char* x311 = CTYPES_PTR_OF_OCAML_BYTES(x306);
   Hacl_Hash_Core_SHA2_pad_256(x308, x311);
   return Val_unit;
}
value _41_Hacl_Hash_Core_SHA2_finish_224(value x314, value x313)
{
   uint32_t* x315 = CTYPES_ADDR_OF_FATPTR(x314);
   unsigned char* x316 = CTYPES_PTR_OF_OCAML_BYTES(x313);
   Hacl_Hash_Core_SHA2_finish_224(x315, x316);
   return Val_unit;
}
value _42_Hacl_Hash_Core_SHA2_finish_256(value x319, value x318)
{
   uint32_t* x320 = CTYPES_ADDR_OF_FATPTR(x319);
   unsigned char* x321 = CTYPES_PTR_OF_OCAML_BYTES(x318);
   Hacl_Hash_Core_SHA2_finish_256(x320, x321);
   return Val_unit;
}
value _43_Hacl_Hash_Core_SHA2_finish_384(value x324, value x323)
{
   uint64_t* x325 = CTYPES_ADDR_OF_FATPTR(x324);
   unsigned char* x326 = CTYPES_PTR_OF_OCAML_BYTES(x323);
   Hacl_Hash_Core_SHA2_finish_384(x325, x326);
   return Val_unit;
}
value _44_Hacl_Hash_Core_SHA2_finish_512(value x329, value x328)
{
   uint64_t* x330 = CTYPES_ADDR_OF_FATPTR(x329);
   unsigned char* x331 = CTYPES_PTR_OF_OCAML_BYTES(x328);
   Hacl_Hash_Core_SHA2_finish_512(x330, x331);
   return Val_unit;
}
value _45_Hacl_Hash_Definitions_word_len(value x333)
{
   uint8_t x334 = Uint8_val(x333);
   uint32_t x337 = Hacl_Hash_Definitions_word_len(x334);
   return integers_copy_uint32(x337);
}
value _46_Hacl_Hash_Definitions_block_len(value x338)
{
   uint8_t x339 = Uint8_val(x338);
   uint32_t x342 = Hacl_Hash_Definitions_block_len(x339);
   return integers_copy_uint32(x342);
}
value _47_Hacl_Hash_Definitions_hash_word_len(value x343)
{
   uint8_t x344 = Uint8_val(x343);
   uint32_t x347 = Hacl_Hash_Definitions_hash_word_len(x344);
   return integers_copy_uint32(x347);
}
value _48_Hacl_Hash_Definitions_hash_len(value x348)
{
   uint8_t x349 = Uint8_val(x348);
   uint32_t x352 = Hacl_Hash_Definitions_hash_len(x349);
   return integers_copy_uint32(x352);
}
