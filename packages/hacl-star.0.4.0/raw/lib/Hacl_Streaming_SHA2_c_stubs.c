
#include "Hacl_Streaming_SHA2.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_SHA2_create_in_224(value x1)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x2 =
   Hacl_Streaming_SHA2_create_in_224();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_Streaming_SHA2_init_224(value x3)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x4 =
   CTYPES_ADDR_OF_FATPTR(x3);
   Hacl_Streaming_SHA2_init_224(x4);
   return Val_unit;
}
value _3_Hacl_Streaming_SHA2_update_224(value x8, value x7, value x6)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x9 =
   CTYPES_ADDR_OF_FATPTR(x8);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   uint32_t x11 = Uint32_val(x6);
   Hacl_Streaming_SHA2_update_224(x9, x10, x11);
   return Val_unit;
}
value _4_Hacl_Streaming_SHA2_finish_224(value x16, value x15)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x17 =
   CTYPES_ADDR_OF_FATPTR(x16);
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   Hacl_Streaming_SHA2_finish_224(x17, x18);
   return Val_unit;
}
value _5_Hacl_Streaming_SHA2_free_224(value x20)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x21 =
   CTYPES_ADDR_OF_FATPTR(x20);
   Hacl_Streaming_SHA2_free_224(x21);
   return Val_unit;
}
value _6_Hacl_Streaming_SHA2_create_in_256(value x23)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x24 =
   Hacl_Streaming_SHA2_create_in_256();
   return CTYPES_FROM_PTR(x24);
}
value _7_Hacl_Streaming_SHA2_init_256(value x25)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x26 =
   CTYPES_ADDR_OF_FATPTR(x25);
   Hacl_Streaming_SHA2_init_256(x26);
   return Val_unit;
}
value _8_Hacl_Streaming_SHA2_update_256(value x30, value x29, value x28)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x31 =
   CTYPES_ADDR_OF_FATPTR(x30);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   uint32_t x33 = Uint32_val(x28);
   Hacl_Streaming_SHA2_update_256(x31, x32, x33);
   return Val_unit;
}
value _9_Hacl_Streaming_SHA2_finish_256(value x38, value x37)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x39 =
   CTYPES_ADDR_OF_FATPTR(x38);
   unsigned char* x40 = CTYPES_PTR_OF_OCAML_BYTES(x37);
   Hacl_Streaming_SHA2_finish_256(x39, x40);
   return Val_unit;
}
value _10_Hacl_Streaming_SHA2_free_256(value x42)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x43 =
   CTYPES_ADDR_OF_FATPTR(x42);
   Hacl_Streaming_SHA2_free_256(x43);
   return Val_unit;
}
value _11_Hacl_Streaming_SHA2_create_in_384(value x45)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x46 =
   Hacl_Streaming_SHA2_create_in_384();
   return CTYPES_FROM_PTR(x46);
}
value _12_Hacl_Streaming_SHA2_init_384(value x47)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x48 =
   CTYPES_ADDR_OF_FATPTR(x47);
   Hacl_Streaming_SHA2_init_384(x48);
   return Val_unit;
}
value _13_Hacl_Streaming_SHA2_update_384(value x52, value x51, value x50)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x53 =
   CTYPES_ADDR_OF_FATPTR(x52);
   unsigned char* x54 = CTYPES_PTR_OF_OCAML_BYTES(x51);
   uint32_t x55 = Uint32_val(x50);
   Hacl_Streaming_SHA2_update_384(x53, x54, x55);
   return Val_unit;
}
value _14_Hacl_Streaming_SHA2_finish_384(value x60, value x59)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x61 =
   CTYPES_ADDR_OF_FATPTR(x60);
   unsigned char* x62 = CTYPES_PTR_OF_OCAML_BYTES(x59);
   Hacl_Streaming_SHA2_finish_384(x61, x62);
   return Val_unit;
}
value _15_Hacl_Streaming_SHA2_free_384(value x64)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x65 =
   CTYPES_ADDR_OF_FATPTR(x64);
   Hacl_Streaming_SHA2_free_384(x65);
   return Val_unit;
}
value _16_Hacl_Streaming_SHA2_create_in_512(value x67)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x68 =
   Hacl_Streaming_SHA2_create_in_512();
   return CTYPES_FROM_PTR(x68);
}
value _17_Hacl_Streaming_SHA2_init_512(value x69)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x70 =
   CTYPES_ADDR_OF_FATPTR(x69);
   Hacl_Streaming_SHA2_init_512(x70);
   return Val_unit;
}
value _18_Hacl_Streaming_SHA2_update_512(value x74, value x73, value x72)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x75 =
   CTYPES_ADDR_OF_FATPTR(x74);
   unsigned char* x76 = CTYPES_PTR_OF_OCAML_BYTES(x73);
   uint32_t x77 = Uint32_val(x72);
   Hacl_Streaming_SHA2_update_512(x75, x76, x77);
   return Val_unit;
}
value _19_Hacl_Streaming_SHA2_finish_512(value x82, value x81)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x83 =
   CTYPES_ADDR_OF_FATPTR(x82);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x81);
   Hacl_Streaming_SHA2_finish_512(x83, x84);
   return Val_unit;
}
value _20_Hacl_Streaming_SHA2_free_512(value x86)
{
   struct Hacl_Streaming_SHA2_state_sha2_384_s* x87 =
   CTYPES_ADDR_OF_FATPTR(x86);
   Hacl_Streaming_SHA2_free_512(x87);
   return Val_unit;
}
