
#include "Hacl_Streaming_Blake2.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_Blake2_blake2s_32_no_key_create_in(value x1)
{
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x2
   = Hacl_Streaming_Blake2_blake2s_32_no_key_create_in();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_Streaming_Blake2_blake2s_32_no_key_update(value x5, value x4,
                                                        value x3)
{
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x6
   = CTYPES_ADDR_OF_FATPTR(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x8 = Uint32_val(x3);
   Hacl_Streaming_Blake2_blake2s_32_no_key_update(x6, x7, x8);
   return Val_unit;
}
value _3_Hacl_Streaming_Blake2_blake2s_32_no_key_finish(value x13, value x12)
{
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x14
   = CTYPES_ADDR_OF_FATPTR(x13);
   unsigned char* x15 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   Hacl_Streaming_Blake2_blake2s_32_no_key_finish(x14, x15);
   return Val_unit;
}
value _4_Hacl_Streaming_Blake2_blake2s_32_no_key_free(value x17)
{
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x18
   = CTYPES_ADDR_OF_FATPTR(x17);
   Hacl_Streaming_Blake2_blake2s_32_no_key_free(x18);
   return Val_unit;
}
value _5_Hacl_Streaming_Blake2_blake2b_32_no_key_create_in(value x20)
{
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x21
   = Hacl_Streaming_Blake2_blake2b_32_no_key_create_in();
   return CTYPES_FROM_PTR(x21);
}
value _6_Hacl_Streaming_Blake2_blake2b_32_no_key_update(value x24, value x23,
                                                        value x22)
{
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x25
   = CTYPES_ADDR_OF_FATPTR(x24);
   unsigned char* x26 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   uint32_t x27 = Uint32_val(x22);
   Hacl_Streaming_Blake2_blake2b_32_no_key_update(x25, x26, x27);
   return Val_unit;
}
value _7_Hacl_Streaming_Blake2_blake2b_32_no_key_finish(value x32, value x31)
{
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x33
   = CTYPES_ADDR_OF_FATPTR(x32);
   unsigned char* x34 = CTYPES_PTR_OF_OCAML_BYTES(x31);
   Hacl_Streaming_Blake2_blake2b_32_no_key_finish(x33, x34);
   return Val_unit;
}
value _8_Hacl_Streaming_Blake2_blake2b_32_no_key_free(value x36)
{
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x37
   = CTYPES_ADDR_OF_FATPTR(x36);
   Hacl_Streaming_Blake2_blake2b_32_no_key_free(x37);
   return Val_unit;
}
value _9_Hacl_Streaming_Blake2_blake2s_32_with_key_create_in(value x40,
                                                             value x39)
{
   uint32_t x41 = Uint32_val(x40);
   unsigned char* x44 = CTYPES_PTR_OF_OCAML_BYTES(x39);
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x45
   = Hacl_Streaming_Blake2_blake2s_32_with_key_create_in(x41, x44);
   return CTYPES_FROM_PTR(x45);
}
value _10_Hacl_Streaming_Blake2_blake2s_32_with_key_update(value x49,
                                                           value x48,
                                                           value x47,
                                                           value x46)
{
   uint32_t x50 = Uint32_val(x49);
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x53
   = CTYPES_ADDR_OF_FATPTR(x48);
   unsigned char* x54 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   uint32_t x55 = Uint32_val(x46);
   Hacl_Streaming_Blake2_blake2s_32_with_key_update(x50, x53, x54, x55);
   return Val_unit;
}
value _11_Hacl_Streaming_Blake2_blake2s_32_with_key_finish(value x61,
                                                           value x60,
                                                           value x59)
{
   uint32_t x62 = Uint32_val(x61);
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x65
   = CTYPES_ADDR_OF_FATPTR(x60);
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x59);
   Hacl_Streaming_Blake2_blake2s_32_with_key_finish(x62, x65, x66);
   return Val_unit;
}
value _12_Hacl_Streaming_Blake2_blake2s_32_with_key_free(value x69,
                                                         value x68)
{
   uint32_t x70 = Uint32_val(x69);
   struct Hacl_Streaming_Functor_state_s__K____uint32_t___uint32_t_____s* x73
   = CTYPES_ADDR_OF_FATPTR(x68);
   Hacl_Streaming_Blake2_blake2s_32_with_key_free(x70, x73);
   return Val_unit;
}
value _13_Hacl_Streaming_Blake2_blake2b_32_with_key_create_in(value x76,
                                                              value x75)
{
   uint32_t x77 = Uint32_val(x76);
   unsigned char* x80 = CTYPES_PTR_OF_OCAML_BYTES(x75);
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x81
   = Hacl_Streaming_Blake2_blake2b_32_with_key_create_in(x77, x80);
   return CTYPES_FROM_PTR(x81);
}
value _14_Hacl_Streaming_Blake2_blake2b_32_with_key_update(value x85,
                                                           value x84,
                                                           value x83,
                                                           value x82)
{
   uint32_t x86 = Uint32_val(x85);
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x89
   = CTYPES_ADDR_OF_FATPTR(x84);
   unsigned char* x90 = CTYPES_PTR_OF_OCAML_BYTES(x83);
   uint32_t x91 = Uint32_val(x82);
   Hacl_Streaming_Blake2_blake2b_32_with_key_update(x86, x89, x90, x91);
   return Val_unit;
}
value _15_Hacl_Streaming_Blake2_blake2b_32_with_key_finish(value x97,
                                                           value x96,
                                                           value x95)
{
   uint32_t x98 = Uint32_val(x97);
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x101
   = CTYPES_ADDR_OF_FATPTR(x96);
   unsigned char* x102 = CTYPES_PTR_OF_OCAML_BYTES(x95);
   Hacl_Streaming_Blake2_blake2b_32_with_key_finish(x98, x101, x102);
   return Val_unit;
}
value _16_Hacl_Streaming_Blake2_blake2b_32_with_key_free(value x105,
                                                         value x104)
{
   uint32_t x106 = Uint32_val(x105);
   struct Hacl_Streaming_Functor_state_s__K____uint64_t___uint64_t_____s* x109
   = CTYPES_ADDR_OF_FATPTR(x104);
   Hacl_Streaming_Blake2_blake2b_32_with_key_free(x106, x109);
   return Val_unit;
}
