
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
   char* x21 = CTYPES_PTR_OF_OCAML_STRING(x18);
   uint32_t x22 = Uint32_val(x17);
   EverCrypt_Hash_update_multi_256(x20, x21, x22);
   return Val_unit;
}
value _6_EverCrypt_Hash_update(value x27, value x26)
{
   struct EverCrypt_Hash_state_s_s* x28 = CTYPES_ADDR_OF_FATPTR(x27);
   char* x29 = CTYPES_PTR_OF_OCAML_STRING(x26);
   EverCrypt_Hash_update(x28, x29);
   return Val_unit;
}
value _7_EverCrypt_Hash_update_multi(value x33, value x32, value x31)
{
   struct EverCrypt_Hash_state_s_s* x34 = CTYPES_ADDR_OF_FATPTR(x33);
   char* x35 = CTYPES_PTR_OF_OCAML_STRING(x32);
   uint32_t x36 = Uint32_val(x31);
   EverCrypt_Hash_update_multi(x34, x35, x36);
   return Val_unit;
}
value _8_EverCrypt_Hash_update_last_256(value x43, value x42, value x41,
                                        value x40)
{
   uint32_t* x44 = CTYPES_ADDR_OF_FATPTR(x43);
   uint64_t x45 = Uint64_val(x42);
   char* x48 = CTYPES_PTR_OF_OCAML_STRING(x41);
   uint32_t x49 = Uint32_val(x40);
   EverCrypt_Hash_update_last_256(x44, x45, x48, x49);
   return Val_unit;
}
value _9_EverCrypt_Hash_update_last(value x55, value x54, value x53)
{
   struct EverCrypt_Hash_state_s_s* x56 = CTYPES_ADDR_OF_FATPTR(x55);
   char* x57 = CTYPES_PTR_OF_OCAML_STRING(x54);
   uint64_t x58 = Uint64_val(x53);
   EverCrypt_Hash_update_last(x56, x57, x58);
   return Val_unit;
}
value _10_EverCrypt_Hash_finish(value x63, value x62)
{
   struct EverCrypt_Hash_state_s_s* x64 = CTYPES_ADDR_OF_FATPTR(x63);
   char* x65 = CTYPES_PTR_OF_OCAML_STRING(x62);
   EverCrypt_Hash_finish(x64, x65);
   return Val_unit;
}
value _11_EverCrypt_Hash_free(value x67)
{
   struct EverCrypt_Hash_state_s_s* x68 = CTYPES_ADDR_OF_FATPTR(x67);
   EverCrypt_Hash_free(x68);
   return Val_unit;
}
value _12_EverCrypt_Hash_copy(value x71, value x70)
{
   struct EverCrypt_Hash_state_s_s* x72 = CTYPES_ADDR_OF_FATPTR(x71);
   struct EverCrypt_Hash_state_s_s* x73 = CTYPES_ADDR_OF_FATPTR(x70);
   EverCrypt_Hash_copy(x72, x73);
   return Val_unit;
}
value _13_EverCrypt_Hash_hash_256(value x77, value x76, value x75)
{
   char* x78 = CTYPES_PTR_OF_OCAML_STRING(x77);
   uint32_t x79 = Uint32_val(x76);
   char* x82 = CTYPES_PTR_OF_OCAML_STRING(x75);
   EverCrypt_Hash_hash_256(x78, x79, x82);
   return Val_unit;
}
value _14_EverCrypt_Hash_hash_224(value x86, value x85, value x84)
{
   char* x87 = CTYPES_PTR_OF_OCAML_STRING(x86);
   uint32_t x88 = Uint32_val(x85);
   char* x91 = CTYPES_PTR_OF_OCAML_STRING(x84);
   EverCrypt_Hash_hash_224(x87, x88, x91);
   return Val_unit;
}
value _15_EverCrypt_Hash_hash(value x96, value x95, value x94, value x93)
{
   uint8_t x97 = Uint8_val(x96);
   char* x100 = CTYPES_PTR_OF_OCAML_STRING(x95);
   char* x101 = CTYPES_PTR_OF_OCAML_STRING(x94);
   uint32_t x102 = Uint32_val(x93);
   EverCrypt_Hash_hash(x97, x100, x101, x102);
   return Val_unit;
}
value _16_EverCrypt_Hash_Incremental_create_in(value x106)
{
   uint8_t x107 = Uint8_val(x106);
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x110
   = EverCrypt_Hash_Incremental_create_in(x107);
   return CTYPES_FROM_PTR(x110);
}
value _17_EverCrypt_Hash_Incremental_init(value x111)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x112
   = CTYPES_ADDR_OF_FATPTR(x111);
   EverCrypt_Hash_Incremental_init(x112);
   return Val_unit;
}
value _18_EverCrypt_Hash_Incremental_update(value x116, value x115,
                                            value x114)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x117
   = CTYPES_ADDR_OF_FATPTR(x116);
   char* x118 = CTYPES_PTR_OF_OCAML_STRING(x115);
   uint32_t x119 = Uint32_val(x114);
   EverCrypt_Hash_Incremental_update(x117, x118, x119);
   return Val_unit;
}
value _19_EverCrypt_Hash_Incremental_finish_md5(value x124, value x123)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x125
   = CTYPES_ADDR_OF_FATPTR(x124);
   char* x126 = CTYPES_PTR_OF_OCAML_STRING(x123);
   EverCrypt_Hash_Incremental_finish_md5(x125, x126);
   return Val_unit;
}
value _20_EverCrypt_Hash_Incremental_finish_sha1(value x129, value x128)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x130
   = CTYPES_ADDR_OF_FATPTR(x129);
   char* x131 = CTYPES_PTR_OF_OCAML_STRING(x128);
   EverCrypt_Hash_Incremental_finish_sha1(x130, x131);
   return Val_unit;
}
value _21_EverCrypt_Hash_Incremental_finish_sha224(value x134, value x133)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x135
   = CTYPES_ADDR_OF_FATPTR(x134);
   char* x136 = CTYPES_PTR_OF_OCAML_STRING(x133);
   EverCrypt_Hash_Incremental_finish_sha224(x135, x136);
   return Val_unit;
}
value _22_EverCrypt_Hash_Incremental_finish_sha256(value x139, value x138)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x140
   = CTYPES_ADDR_OF_FATPTR(x139);
   char* x141 = CTYPES_PTR_OF_OCAML_STRING(x138);
   EverCrypt_Hash_Incremental_finish_sha256(x140, x141);
   return Val_unit;
}
value _23_EverCrypt_Hash_Incremental_finish_sha384(value x144, value x143)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x145
   = CTYPES_ADDR_OF_FATPTR(x144);
   char* x146 = CTYPES_PTR_OF_OCAML_STRING(x143);
   EverCrypt_Hash_Incremental_finish_sha384(x145, x146);
   return Val_unit;
}
value _24_EverCrypt_Hash_Incremental_finish_sha512(value x149, value x148)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x150
   = CTYPES_ADDR_OF_FATPTR(x149);
   char* x151 = CTYPES_PTR_OF_OCAML_STRING(x148);
   EverCrypt_Hash_Incremental_finish_sha512(x150, x151);
   return Val_unit;
}
value _25_EverCrypt_Hash_Incremental_alg_of_state(value x153)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x154
   = CTYPES_ADDR_OF_FATPTR(x153);
   Spec_Hash_Definitions_hash_alg x155 =
   EverCrypt_Hash_Incremental_alg_of_state(x154);
   return Integers_val_uint8(x155);
}
value _26_EverCrypt_Hash_Incremental_finish(value x157, value x156)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x158
   = CTYPES_ADDR_OF_FATPTR(x157);
   char* x159 = CTYPES_PTR_OF_OCAML_STRING(x156);
   EverCrypt_Hash_Incremental_finish(x158, x159);
   return Val_unit;
}
value _27_EverCrypt_Hash_Incremental_free(value x161)
{
   struct Hacl_Streaming_Functor_state_s___EverCrypt_Hash_state_s_____s* x162
   = CTYPES_ADDR_OF_FATPTR(x161);
   EverCrypt_Hash_Incremental_free(x162);
   return Val_unit;
}
