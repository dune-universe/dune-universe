
#include "Hacl_Streaming_Blake2b_256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_Blake2b_256_blake2b_256_no_key_create_in(value x1)
{
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x2
   = Hacl_Streaming_Blake2b_256_blake2b_256_no_key_create_in();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_Streaming_Blake2b_256_blake2b_256_no_key_update(value x5,
                                                              value x4,
                                                              value x3)
{
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x6
   = CTYPES_ADDR_OF_FATPTR(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x8 = Uint32_val(x3);
   Hacl_Streaming_Blake2b_256_blake2b_256_no_key_update(x6, x7, x8);
   return Val_unit;
}
value _3_Hacl_Streaming_Blake2b_256_blake2b_256_no_key_finish(value x13,
                                                              value x12)
{
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x14
   = CTYPES_ADDR_OF_FATPTR(x13);
   unsigned char* x15 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   Hacl_Streaming_Blake2b_256_blake2b_256_no_key_finish(x14, x15);
   return Val_unit;
}
value _4_Hacl_Streaming_Blake2b_256_blake2b_256_no_key_free(value x17)
{
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x18
   = CTYPES_ADDR_OF_FATPTR(x17);
   Hacl_Streaming_Blake2b_256_blake2b_256_no_key_free(x18);
   return Val_unit;
}
value _5_Hacl_Streaming_Blake2b_256_blake2b_256_with_key_create_in(value x21,
                                                                   value x20)
{
   uint32_t x22 = Uint32_val(x21);
   unsigned char* x25 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x26
   = Hacl_Streaming_Blake2b_256_blake2b_256_with_key_create_in(x22, x25);
   return CTYPES_FROM_PTR(x26);
}
value _6_Hacl_Streaming_Blake2b_256_blake2b_256_with_key_update(value x30,
                                                                value x29,
                                                                value x28,
                                                                value x27)
{
   uint32_t x31 = Uint32_val(x30);
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x34
   = CTYPES_ADDR_OF_FATPTR(x29);
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   uint32_t x36 = Uint32_val(x27);
   Hacl_Streaming_Blake2b_256_blake2b_256_with_key_update(x31, x34, x35, x36);
   return Val_unit;
}
value _7_Hacl_Streaming_Blake2b_256_blake2b_256_with_key_finish(value x42,
                                                                value x41,
                                                                value x40)
{
   uint32_t x43 = Uint32_val(x42);
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x46
   = CTYPES_ADDR_OF_FATPTR(x41);
   unsigned char* x47 = CTYPES_PTR_OF_OCAML_BYTES(x40);
   Hacl_Streaming_Blake2b_256_blake2b_256_with_key_finish(x43, x46, x47);
   return Val_unit;
}
value _8_Hacl_Streaming_Blake2b_256_blake2b_256_with_key_free(value x50,
                                                              value x49)
{
   uint32_t x51 = Uint32_val(x50);
   struct Hacl_Streaming_Functor_state_s__K____Lib_IntVector_Intrinsics_vec256___Lib_IntVector_Intrinsics_vec256_____s* x54
   = CTYPES_ADDR_OF_FATPTR(x49);
   Hacl_Streaming_Blake2b_256_blake2b_256_with_key_free(x51, x54);
   return Val_unit;
}
