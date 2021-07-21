
#include "Hacl_Streaming_Blake2.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_Blake2_blocks_state_len(value x2, value x1)
{
   uint8_t x3 = Uint8_val(x2);
   uint8_t x6 = Uint8_val(x1);
   uint32_t x9 = Hacl_Streaming_Blake2_blocks_state_len(x3, x6);
   return integers_copy_uint32(x9);
}
value _2_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in(value x10)
{
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x11 =
   Hacl_Streaming_Blake2_blake2s_32_no_key_create_in();
   return CTYPES_FROM_PTR(x11);
}
value _3_Hacl_Streaming_Blake2_blake2s_32_no_key_init(value x12)
{
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x13 =
   CTYPES_ADDR_OF_FATPTR(x12);
   Hacl_Streaming_Blake2_blake2s_32_no_key_init(x13);
   return Val_unit;
}
value _4_Hacl_Streaming_Blake2_blake2s_32_no_key_update(value x17, value x16,
                                                        value x15)
{
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x18 =
   CTYPES_ADDR_OF_FATPTR(x17);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x16);
   uint32_t x20 = Uint32_val(x15);
   Hacl_Streaming_Blake2_blake2s_32_no_key_update(x18, x19, x20);
   return Val_unit;
}
value _5_Hacl_Streaming_Blake2_blake2s_32_no_key_finish(value x25, value x24)
{
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x26 =
   CTYPES_ADDR_OF_FATPTR(x25);
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x24);
   Hacl_Streaming_Blake2_blake2s_32_no_key_finish(x26, x27);
   return Val_unit;
}
value _6_Hacl_Streaming_Blake2_blake2s_32_no_key_free(value x29)
{
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x30 =
   CTYPES_ADDR_OF_FATPTR(x29);
   Hacl_Streaming_Blake2_blake2s_32_no_key_free(x30);
   return Val_unit;
}
value _7_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in(value x32)
{
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x33 =
   Hacl_Streaming_Blake2_blake2b_32_no_key_create_in();
   return CTYPES_FROM_PTR(x33);
}
value _8_Hacl_Streaming_Blake2_blake2b_32_no_key_init(value x34)
{
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x35 =
   CTYPES_ADDR_OF_FATPTR(x34);
   Hacl_Streaming_Blake2_blake2b_32_no_key_init(x35);
   return Val_unit;
}
value _9_Hacl_Streaming_Blake2_blake2b_32_no_key_update(value x39, value x38,
                                                        value x37)
{
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x40 =
   CTYPES_ADDR_OF_FATPTR(x39);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x38);
   uint32_t x42 = Uint32_val(x37);
   Hacl_Streaming_Blake2_blake2b_32_no_key_update(x40, x41, x42);
   return Val_unit;
}
value _10_Hacl_Streaming_Blake2_blake2b_32_no_key_finish(value x47,
                                                         value x46)
{
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x48 =
   CTYPES_ADDR_OF_FATPTR(x47);
   unsigned char* x49 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   Hacl_Streaming_Blake2_blake2b_32_no_key_finish(x48, x49);
   return Val_unit;
}
value _11_Hacl_Streaming_Blake2_blake2b_32_no_key_free(value x51)
{
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x52 =
   CTYPES_ADDR_OF_FATPTR(x51);
   Hacl_Streaming_Blake2_blake2b_32_no_key_free(x52);
   return Val_unit;
}
value _12_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in(value x55,
                                                              value x54)
{
   uint32_t x56 = Uint32_val(x55);
   unsigned char* x59 = CTYPES_PTR_OF_OCAML_BYTES(x54);
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x60 =
   Hacl_Streaming_Blake2_blake2s_32_with_key_create_in(x56, x59);
   return CTYPES_FROM_PTR(x60);
}
value _13_Hacl_Streaming_Blake2_blake2s_32_with_key_init(value x63,
                                                         value x62,
                                                         value x61)
{
   uint32_t x64 = Uint32_val(x63);
   unsigned char* x67 = CTYPES_PTR_OF_OCAML_BYTES(x62);
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x68 =
   CTYPES_ADDR_OF_FATPTR(x61);
   Hacl_Streaming_Blake2_blake2s_32_with_key_init(x64, x67, x68);
   return Val_unit;
}
value _14_Hacl_Streaming_Blake2_blake2s_32_with_key_update(value x73,
                                                           value x72,
                                                           value x71,
                                                           value x70)
{
   uint32_t x74 = Uint32_val(x73);
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x77 =
   CTYPES_ADDR_OF_FATPTR(x72);
   unsigned char* x78 = CTYPES_PTR_OF_OCAML_BYTES(x71);
   uint32_t x79 = Uint32_val(x70);
   Hacl_Streaming_Blake2_blake2s_32_with_key_update(x74, x77, x78, x79);
   return Val_unit;
}
value _15_Hacl_Streaming_Blake2_blake2s_32_with_key_finish(value x85,
                                                           value x84,
                                                           value x83)
{
   uint32_t x86 = Uint32_val(x85);
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x89 =
   CTYPES_ADDR_OF_FATPTR(x84);
   unsigned char* x90 = CTYPES_PTR_OF_OCAML_BYTES(x83);
   Hacl_Streaming_Blake2_blake2s_32_with_key_finish(x86, x89, x90);
   return Val_unit;
}
value _16_Hacl_Streaming_Blake2_blake2s_32_with_key_free(value x93,
                                                         value x92)
{
   uint32_t x94 = Uint32_val(x93);
   struct Hacl_Streaming_Blake2_blake2s_32_state_s* x97 =
   CTYPES_ADDR_OF_FATPTR(x92);
   Hacl_Streaming_Blake2_blake2s_32_with_key_free(x94, x97);
   return Val_unit;
}
value _17_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in(value x100,
                                                              value x99)
{
   uint32_t x101 = Uint32_val(x100);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x99);
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x105 =
   Hacl_Streaming_Blake2_blake2b_32_with_key_create_in(x101, x104);
   return CTYPES_FROM_PTR(x105);
}
value _18_Hacl_Streaming_Blake2_blake2b_32_with_key_init(value x108,
                                                         value x107,
                                                         value x106)
{
   uint32_t x109 = Uint32_val(x108);
   unsigned char* x112 = CTYPES_PTR_OF_OCAML_BYTES(x107);
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x113 =
   CTYPES_ADDR_OF_FATPTR(x106);
   Hacl_Streaming_Blake2_blake2b_32_with_key_init(x109, x112, x113);
   return Val_unit;
}
value _19_Hacl_Streaming_Blake2_blake2b_32_with_key_update(value x118,
                                                           value x117,
                                                           value x116,
                                                           value x115)
{
   uint32_t x119 = Uint32_val(x118);
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x122 =
   CTYPES_ADDR_OF_FATPTR(x117);
   unsigned char* x123 = CTYPES_PTR_OF_OCAML_BYTES(x116);
   uint32_t x124 = Uint32_val(x115);
   Hacl_Streaming_Blake2_blake2b_32_with_key_update(x119, x122, x123, x124);
   return Val_unit;
}
value _20_Hacl_Streaming_Blake2_blake2b_32_with_key_finish(value x130,
                                                           value x129,
                                                           value x128)
{
   uint32_t x131 = Uint32_val(x130);
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x134 =
   CTYPES_ADDR_OF_FATPTR(x129);
   unsigned char* x135 = CTYPES_PTR_OF_OCAML_BYTES(x128);
   Hacl_Streaming_Blake2_blake2b_32_with_key_finish(x131, x134, x135);
   return Val_unit;
}
value _21_Hacl_Streaming_Blake2_blake2b_32_with_key_free(value x138,
                                                         value x137)
{
   uint32_t x139 = Uint32_val(x138);
   struct Hacl_Streaming_Blake2_blake2b_32_state_s* x142 =
   CTYPES_ADDR_OF_FATPTR(x137);
   Hacl_Streaming_Blake2_blake2b_32_with_key_free(x139, x142);
   return Val_unit;
}
