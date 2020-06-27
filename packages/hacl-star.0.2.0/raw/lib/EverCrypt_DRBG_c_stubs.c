
#include "EverCrypt_DRBG.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_DRBG_reseed_interval(value _)
{
   uint32_t* x1 = &EverCrypt_DRBG_reseed_interval;
   return CTYPES_FROM_PTR(x1);
}
value _2_EverCrypt_DRBG_max_output_length(value _)
{
   uint32_t* x2 = &EverCrypt_DRBG_max_output_length;
   return CTYPES_FROM_PTR(x2);
}
value _3_EverCrypt_DRBG_max_length(value _)
{
   uint32_t* x3 = &EverCrypt_DRBG_max_length;
   return CTYPES_FROM_PTR(x3);
}
value _4_EverCrypt_DRBG_max_personalization_string_length(value _)
{
   uint32_t* x4 = &EverCrypt_DRBG_max_personalization_string_length;
   return CTYPES_FROM_PTR(x4);
}
value _5_EverCrypt_DRBG_max_additional_input_length(value _)
{
   uint32_t* x5 = &EverCrypt_DRBG_max_additional_input_length;
   return CTYPES_FROM_PTR(x5);
}
value _6_EverCrypt_DRBG_min_length(value x6)
{
   uint8_t x7 = Uint8_val(x6);
   uint32_t x10 = EverCrypt_DRBG_min_length(x7);
   return integers_copy_uint32(x10);
}
value _7_EverCrypt_DRBG_create(value x11)
{
   uint8_t x12 = Uint8_val(x11);
   struct EverCrypt_DRBG_state_s_s* x15 = EverCrypt_DRBG_create(x12);
   return CTYPES_FROM_PTR(x15);
}
value _8_EverCrypt_DRBG_instantiate_sha1(value x18, value x17, value x16)
{
   struct EverCrypt_DRBG_state_s_s* x19 = CTYPES_ADDR_OF_FATPTR(x18);
   unsigned char* x20 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x21 = Uint32_val(x16);
   _Bool x24 = EverCrypt_DRBG_instantiate_sha1(x19, x20, x21);
   return Val_bool(x24);
}
value _9_EverCrypt_DRBG_instantiate_sha2_256(value x27, value x26, value x25)
{
   struct EverCrypt_DRBG_state_s_s* x28 = CTYPES_ADDR_OF_FATPTR(x27);
   unsigned char* x29 = CTYPES_PTR_OF_OCAML_BYTES(x26);
   uint32_t x30 = Uint32_val(x25);
   _Bool x33 = EverCrypt_DRBG_instantiate_sha2_256(x28, x29, x30);
   return Val_bool(x33);
}
value _10_EverCrypt_DRBG_instantiate_sha2_384(value x36, value x35,
                                              value x34)
{
   struct EverCrypt_DRBG_state_s_s* x37 = CTYPES_ADDR_OF_FATPTR(x36);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   uint32_t x39 = Uint32_val(x34);
   _Bool x42 = EverCrypt_DRBG_instantiate_sha2_384(x37, x38, x39);
   return Val_bool(x42);
}
value _11_EverCrypt_DRBG_instantiate_sha2_512(value x45, value x44,
                                              value x43)
{
   struct EverCrypt_DRBG_state_s_s* x46 = CTYPES_ADDR_OF_FATPTR(x45);
   unsigned char* x47 = CTYPES_PTR_OF_OCAML_BYTES(x44);
   uint32_t x48 = Uint32_val(x43);
   _Bool x51 = EverCrypt_DRBG_instantiate_sha2_512(x46, x47, x48);
   return Val_bool(x51);
}
value _12_EverCrypt_DRBG_reseed_sha1(value x54, value x53, value x52)
{
   struct EverCrypt_DRBG_state_s_s* x55 = CTYPES_ADDR_OF_FATPTR(x54);
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x53);
   uint32_t x57 = Uint32_val(x52);
   _Bool x60 = EverCrypt_DRBG_reseed_sha1(x55, x56, x57);
   return Val_bool(x60);
}
value _13_EverCrypt_DRBG_reseed_sha2_256(value x63, value x62, value x61)
{
   struct EverCrypt_DRBG_state_s_s* x64 = CTYPES_ADDR_OF_FATPTR(x63);
   unsigned char* x65 = CTYPES_PTR_OF_OCAML_BYTES(x62);
   uint32_t x66 = Uint32_val(x61);
   _Bool x69 = EverCrypt_DRBG_reseed_sha2_256(x64, x65, x66);
   return Val_bool(x69);
}
value _14_EverCrypt_DRBG_reseed_sha2_384(value x72, value x71, value x70)
{
   struct EverCrypt_DRBG_state_s_s* x73 = CTYPES_ADDR_OF_FATPTR(x72);
   unsigned char* x74 = CTYPES_PTR_OF_OCAML_BYTES(x71);
   uint32_t x75 = Uint32_val(x70);
   _Bool x78 = EverCrypt_DRBG_reseed_sha2_384(x73, x74, x75);
   return Val_bool(x78);
}
value _15_EverCrypt_DRBG_reseed_sha2_512(value x81, value x80, value x79)
{
   struct EverCrypt_DRBG_state_s_s* x82 = CTYPES_ADDR_OF_FATPTR(x81);
   unsigned char* x83 = CTYPES_PTR_OF_OCAML_BYTES(x80);
   uint32_t x84 = Uint32_val(x79);
   _Bool x87 = EverCrypt_DRBG_reseed_sha2_512(x82, x83, x84);
   return Val_bool(x87);
}
value _16_EverCrypt_DRBG_generate_sha1(value x92, value x91, value x90,
                                       value x89, value x88)
{
   unsigned char* x93 = CTYPES_PTR_OF_OCAML_BYTES(x92);
   struct EverCrypt_DRBG_state_s_s* x94 = CTYPES_ADDR_OF_FATPTR(x91);
   uint32_t x95 = Uint32_val(x90);
   unsigned char* x98 = CTYPES_PTR_OF_OCAML_BYTES(x89);
   uint32_t x99 = Uint32_val(x88);
   _Bool x102 = EverCrypt_DRBG_generate_sha1(x93, x94, x95, x98, x99);
   return Val_bool(x102);
}
value _17_EverCrypt_DRBG_generate_sha2_256(value x107, value x106,
                                           value x105, value x104,
                                           value x103)
{
   unsigned char* x108 = CTYPES_PTR_OF_OCAML_BYTES(x107);
   struct EverCrypt_DRBG_state_s_s* x109 = CTYPES_ADDR_OF_FATPTR(x106);
   uint32_t x110 = Uint32_val(x105);
   unsigned char* x113 = CTYPES_PTR_OF_OCAML_BYTES(x104);
   uint32_t x114 = Uint32_val(x103);
   _Bool x117 =
   EverCrypt_DRBG_generate_sha2_256(x108, x109, x110, x113, x114);
   return Val_bool(x117);
}
value _18_EverCrypt_DRBG_generate_sha2_384(value x122, value x121,
                                           value x120, value x119,
                                           value x118)
{
   unsigned char* x123 = CTYPES_PTR_OF_OCAML_BYTES(x122);
   struct EverCrypt_DRBG_state_s_s* x124 = CTYPES_ADDR_OF_FATPTR(x121);
   uint32_t x125 = Uint32_val(x120);
   unsigned char* x128 = CTYPES_PTR_OF_OCAML_BYTES(x119);
   uint32_t x129 = Uint32_val(x118);
   _Bool x132 =
   EverCrypt_DRBG_generate_sha2_384(x123, x124, x125, x128, x129);
   return Val_bool(x132);
}
value _19_EverCrypt_DRBG_generate_sha2_512(value x137, value x136,
                                           value x135, value x134,
                                           value x133)
{
   unsigned char* x138 = CTYPES_PTR_OF_OCAML_BYTES(x137);
   struct EverCrypt_DRBG_state_s_s* x139 = CTYPES_ADDR_OF_FATPTR(x136);
   uint32_t x140 = Uint32_val(x135);
   unsigned char* x143 = CTYPES_PTR_OF_OCAML_BYTES(x134);
   uint32_t x144 = Uint32_val(x133);
   _Bool x147 =
   EverCrypt_DRBG_generate_sha2_512(x138, x139, x140, x143, x144);
   return Val_bool(x147);
}
value _20_EverCrypt_DRBG_uninstantiate_sha1(value x148)
{
   struct EverCrypt_DRBG_state_s_s* x149 = CTYPES_ADDR_OF_FATPTR(x148);
   EverCrypt_DRBG_uninstantiate_sha1(x149);
   return Val_unit;
}
value _21_EverCrypt_DRBG_uninstantiate_sha2_256(value x151)
{
   struct EverCrypt_DRBG_state_s_s* x152 = CTYPES_ADDR_OF_FATPTR(x151);
   EverCrypt_DRBG_uninstantiate_sha2_256(x152);
   return Val_unit;
}
value _22_EverCrypt_DRBG_uninstantiate_sha2_384(value x154)
{
   struct EverCrypt_DRBG_state_s_s* x155 = CTYPES_ADDR_OF_FATPTR(x154);
   EverCrypt_DRBG_uninstantiate_sha2_384(x155);
   return Val_unit;
}
value _23_EverCrypt_DRBG_uninstantiate_sha2_512(value x157)
{
   struct EverCrypt_DRBG_state_s_s* x158 = CTYPES_ADDR_OF_FATPTR(x157);
   EverCrypt_DRBG_uninstantiate_sha2_512(x158);
   return Val_unit;
}
value _24_EverCrypt_DRBG_instantiate(value x162, value x161, value x160)
{
   struct EverCrypt_DRBG_state_s_s* x163 = CTYPES_ADDR_OF_FATPTR(x162);
   unsigned char* x164 = CTYPES_PTR_OF_OCAML_BYTES(x161);
   uint32_t x165 = Uint32_val(x160);
   _Bool x168 = EverCrypt_DRBG_instantiate(x163, x164, x165);
   return Val_bool(x168);
}
value _25_EverCrypt_DRBG_reseed(value x171, value x170, value x169)
{
   struct EverCrypt_DRBG_state_s_s* x172 = CTYPES_ADDR_OF_FATPTR(x171);
   unsigned char* x173 = CTYPES_PTR_OF_OCAML_BYTES(x170);
   uint32_t x174 = Uint32_val(x169);
   _Bool x177 = EverCrypt_DRBG_reseed(x172, x173, x174);
   return Val_bool(x177);
}
value _26_EverCrypt_DRBG_generate(value x182, value x181, value x180,
                                  value x179, value x178)
{
   unsigned char* x183 = CTYPES_PTR_OF_OCAML_BYTES(x182);
   struct EverCrypt_DRBG_state_s_s* x184 = CTYPES_ADDR_OF_FATPTR(x181);
   uint32_t x185 = Uint32_val(x180);
   unsigned char* x188 = CTYPES_PTR_OF_OCAML_BYTES(x179);
   uint32_t x189 = Uint32_val(x178);
   _Bool x192 = EverCrypt_DRBG_generate(x183, x184, x185, x188, x189);
   return Val_bool(x192);
}
value _27_EverCrypt_DRBG_uninstantiate(value x193)
{
   struct EverCrypt_DRBG_state_s_s* x194 = CTYPES_ADDR_OF_FATPTR(x193);
   EverCrypt_DRBG_uninstantiate(x194);
   return Val_unit;
}
