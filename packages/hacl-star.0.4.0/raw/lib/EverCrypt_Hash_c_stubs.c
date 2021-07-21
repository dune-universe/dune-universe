
#include "EverCrypt_Hash.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Hash_alg_of_state(value x1)
{
   struct EverCrypt_Hash_state_s_s* x2 = CTYPES_ADDR_OF_FATPTR(x1);
   Spec_Hash_Definitions_hash_alg x3 = EverCrypt_Hash_alg_of_state(x2);
   return Integers_val_uint8(x3);
}
value _2_EverCrypt_Hash_create_in(value x4)
{
   uint8_t x5 = Uint8_val(x4);
   struct EverCrypt_Hash_state_s_s* x8 = EverCrypt_Hash_create_in(x5);
   return CTYPES_FROM_PTR(x8);
}
value _3_EverCrypt_Hash_create(value x9)
{
   uint8_t x10 = Uint8_val(x9);
   struct EverCrypt_Hash_state_s_s* x13 = EverCrypt_Hash_create(x10);
   return CTYPES_FROM_PTR(x13);
}
value _4_EverCrypt_Hash_init(value x14)
{
   struct EverCrypt_Hash_state_s_s* x15 = CTYPES_ADDR_OF_FATPTR(x14);
   EverCrypt_Hash_init(x15);
   return Val_unit;
}
value _5_EverCrypt_Hash_update_multi_256(value x19, value x18, value x17)
{
   uint32_t* x20 = CTYPES_ADDR_OF_FATPTR(x19);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   uint32_t x22 = Uint32_val(x17);
   EverCrypt_Hash_update_multi_256(x20, x21, x22);
   return Val_unit;
}
value _6_EverCrypt_Hash_update2(value x28, value x27, value x26)
{
   struct EverCrypt_Hash_state_s_s* x29 = CTYPES_ADDR_OF_FATPTR(x28);
   uint64_t x30 = Uint64_val(x27);
   unsigned char* x33 = CTYPES_PTR_OF_OCAML_BYTES(x26);
   EverCrypt_Hash_update2(x29, x30, x33);
   return Val_unit;
}
value _7_EverCrypt_Hash_update(value x36, value x35)
{
   struct EverCrypt_Hash_state_s_s* x37 = CTYPES_ADDR_OF_FATPTR(x36);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   EverCrypt_Hash_update(x37, x38);
   return Val_unit;
}
value _8_EverCrypt_Hash_update_multi2(value x43, value x42, value x41,
                                      value x40)
{
   struct EverCrypt_Hash_state_s_s* x44 = CTYPES_ADDR_OF_FATPTR(x43);
   uint64_t x45 = Uint64_val(x42);
   unsigned char* x48 = CTYPES_PTR_OF_OCAML_BYTES(x41);
   uint32_t x49 = Uint32_val(x40);
   EverCrypt_Hash_update_multi2(x44, x45, x48, x49);
   return Val_unit;
}
value _9_EverCrypt_Hash_update_multi(value x55, value x54, value x53)
{
   struct EverCrypt_Hash_state_s_s* x56 = CTYPES_ADDR_OF_FATPTR(x55);
   unsigned char* x57 = CTYPES_PTR_OF_OCAML_BYTES(x54);
   uint32_t x58 = Uint32_val(x53);
   EverCrypt_Hash_update_multi(x56, x57, x58);
   return Val_unit;
}
value _10_EverCrypt_Hash_update_last_256(value x65, value x64, value x63,
                                         value x62)
{
   uint32_t* x66 = CTYPES_ADDR_OF_FATPTR(x65);
   uint64_t x67 = Uint64_val(x64);
   unsigned char* x70 = CTYPES_PTR_OF_OCAML_BYTES(x63);
   uint32_t x71 = Uint32_val(x62);
   EverCrypt_Hash_update_last_256(x66, x67, x70, x71);
   return Val_unit;
}
value _11_EverCrypt_Hash_update_last2(value x78, value x77, value x76,
                                      value x75)
{
   struct EverCrypt_Hash_state_s_s* x79 = CTYPES_ADDR_OF_FATPTR(x78);
   uint64_t x80 = Uint64_val(x77);
   unsigned char* x83 = CTYPES_PTR_OF_OCAML_BYTES(x76);
   uint32_t x84 = Uint32_val(x75);
   EverCrypt_Hash_update_last2(x79, x80, x83, x84);
   return Val_unit;
}
value _12_EverCrypt_Hash_update_last(value x90, value x89, value x88)
{
   struct EverCrypt_Hash_state_s_s* x91 = CTYPES_ADDR_OF_FATPTR(x90);
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x89);
   uint64_t x93 = Uint64_val(x88);
   EverCrypt_Hash_update_last(x91, x92, x93);
   return Val_unit;
}
value _13_EverCrypt_Hash_finish(value x98, value x97)
{
   struct EverCrypt_Hash_state_s_s* x99 = CTYPES_ADDR_OF_FATPTR(x98);
   unsigned char* x100 = CTYPES_PTR_OF_OCAML_BYTES(x97);
   EverCrypt_Hash_finish(x99, x100);
   return Val_unit;
}
value _14_EverCrypt_Hash_free(value x102)
{
   struct EverCrypt_Hash_state_s_s* x103 = CTYPES_ADDR_OF_FATPTR(x102);
   EverCrypt_Hash_free(x103);
   return Val_unit;
}
value _15_EverCrypt_Hash_copy(value x106, value x105)
{
   struct EverCrypt_Hash_state_s_s* x107 = CTYPES_ADDR_OF_FATPTR(x106);
   struct EverCrypt_Hash_state_s_s* x108 = CTYPES_ADDR_OF_FATPTR(x105);
   EverCrypt_Hash_copy(x107, x108);
   return Val_unit;
}
value _16_EverCrypt_Hash_hash_256(value x112, value x111, value x110)
{
   unsigned char* x113 = CTYPES_PTR_OF_OCAML_BYTES(x112);
   uint32_t x114 = Uint32_val(x111);
   unsigned char* x117 = CTYPES_PTR_OF_OCAML_BYTES(x110);
   EverCrypt_Hash_hash_256(x113, x114, x117);
   return Val_unit;
}
value _17_EverCrypt_Hash_hash_224(value x121, value x120, value x119)
{
   unsigned char* x122 = CTYPES_PTR_OF_OCAML_BYTES(x121);
   uint32_t x123 = Uint32_val(x120);
   unsigned char* x126 = CTYPES_PTR_OF_OCAML_BYTES(x119);
   EverCrypt_Hash_hash_224(x122, x123, x126);
   return Val_unit;
}
value _18_EverCrypt_Hash_hash(value x131, value x130, value x129, value x128)
{
   uint8_t x132 = Uint8_val(x131);
   unsigned char* x135 = CTYPES_PTR_OF_OCAML_BYTES(x130);
   unsigned char* x136 = CTYPES_PTR_OF_OCAML_BYTES(x129);
   uint32_t x137 = Uint32_val(x128);
   EverCrypt_Hash_hash(x132, x135, x136, x137);
   return Val_unit;
}
value _19_EverCrypt_Hash_Incremental_hash_len(value x141)
{
   uint8_t x142 = Uint8_val(x141);
   uint32_t x145 = EverCrypt_Hash_Incremental_hash_len(x142);
   return integers_copy_uint32(x145);
}
value _20_EverCrypt_Hash_Incremental_block_len(value x146)
{
   uint8_t x147 = Uint8_val(x146);
   uint32_t x150 = EverCrypt_Hash_Incremental_block_len(x147);
   return integers_copy_uint32(x150);
}
value _21_EverCrypt_Hash_Incremental_create_in(value x151)
{
   uint8_t x152 = Uint8_val(x151);
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x155
   = EverCrypt_Hash_Incremental_create_in(x152);
   return CTYPES_FROM_PTR(x155);
}
value _22_EverCrypt_Hash_Incremental_init(value x156)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x157
   = CTYPES_ADDR_OF_FATPTR(x156);
   EverCrypt_Hash_Incremental_init(x157);
   return Val_unit;
}
value _23_EverCrypt_Hash_Incremental_update(value x161, value x160,
                                            value x159)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x162
   = CTYPES_ADDR_OF_FATPTR(x161);
   unsigned char* x163 = CTYPES_PTR_OF_OCAML_BYTES(x160);
   uint32_t x164 = Uint32_val(x159);
   EverCrypt_Hash_Incremental_update(x162, x163, x164);
   return Val_unit;
}
value _24_EverCrypt_Hash_Incremental_finish_md5(value x169, value x168)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x170
   = CTYPES_ADDR_OF_FATPTR(x169);
   unsigned char* x171 = CTYPES_PTR_OF_OCAML_BYTES(x168);
   EverCrypt_Hash_Incremental_finish_md5(x170, x171);
   return Val_unit;
}
value _25_EverCrypt_Hash_Incremental_finish_sha1(value x174, value x173)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x175
   = CTYPES_ADDR_OF_FATPTR(x174);
   unsigned char* x176 = CTYPES_PTR_OF_OCAML_BYTES(x173);
   EverCrypt_Hash_Incremental_finish_sha1(x175, x176);
   return Val_unit;
}
value _26_EverCrypt_Hash_Incremental_finish_sha224(value x179, value x178)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x180
   = CTYPES_ADDR_OF_FATPTR(x179);
   unsigned char* x181 = CTYPES_PTR_OF_OCAML_BYTES(x178);
   EverCrypt_Hash_Incremental_finish_sha224(x180, x181);
   return Val_unit;
}
value _27_EverCrypt_Hash_Incremental_finish_sha256(value x184, value x183)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x185
   = CTYPES_ADDR_OF_FATPTR(x184);
   unsigned char* x186 = CTYPES_PTR_OF_OCAML_BYTES(x183);
   EverCrypt_Hash_Incremental_finish_sha256(x185, x186);
   return Val_unit;
}
value _28_EverCrypt_Hash_Incremental_finish_sha384(value x189, value x188)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x190
   = CTYPES_ADDR_OF_FATPTR(x189);
   unsigned char* x191 = CTYPES_PTR_OF_OCAML_BYTES(x188);
   EverCrypt_Hash_Incremental_finish_sha384(x190, x191);
   return Val_unit;
}
value _29_EverCrypt_Hash_Incremental_finish_sha512(value x194, value x193)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x195
   = CTYPES_ADDR_OF_FATPTR(x194);
   unsigned char* x196 = CTYPES_PTR_OF_OCAML_BYTES(x193);
   EverCrypt_Hash_Incremental_finish_sha512(x195, x196);
   return Val_unit;
}
value _30_EverCrypt_Hash_Incremental_finish_blake2s(value x199, value x198)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x200
   = CTYPES_ADDR_OF_FATPTR(x199);
   unsigned char* x201 = CTYPES_PTR_OF_OCAML_BYTES(x198);
   EverCrypt_Hash_Incremental_finish_blake2s(x200, x201);
   return Val_unit;
}
value _31_EverCrypt_Hash_Incremental_finish_blake2b(value x204, value x203)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x205
   = CTYPES_ADDR_OF_FATPTR(x204);
   unsigned char* x206 = CTYPES_PTR_OF_OCAML_BYTES(x203);
   EverCrypt_Hash_Incremental_finish_blake2b(x205, x206);
   return Val_unit;
}
value _32_EverCrypt_Hash_Incremental_alg_of_state(value x208)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x209
   = CTYPES_ADDR_OF_FATPTR(x208);
   Spec_Hash_Definitions_hash_alg x210 =
   EverCrypt_Hash_Incremental_alg_of_state(x209);
   return Integers_val_uint8(x210);
}
value _33_EverCrypt_Hash_Incremental_finish(value x212, value x211)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x213
   = CTYPES_ADDR_OF_FATPTR(x212);
   unsigned char* x214 = CTYPES_PTR_OF_OCAML_BYTES(x211);
   EverCrypt_Hash_Incremental_finish(x213, x214);
   return Val_unit;
}
value _34_EverCrypt_Hash_Incremental_free(value x216)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x217
   = CTYPES_ADDR_OF_FATPTR(x216);
   EverCrypt_Hash_Incremental_free(x217);
   return Val_unit;
}
