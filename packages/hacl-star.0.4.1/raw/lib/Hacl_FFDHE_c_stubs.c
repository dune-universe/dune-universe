
#include "Hacl_FFDHE.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_FFDHE_ffdhe_len(value x1)
{
   uint8_t x2 = Uint8_val(x1);
   uint32_t x5 = Hacl_FFDHE_ffdhe_len(x2);
   return integers_copy_uint32(x5);
}
value _2_Hacl_FFDHE_new_ffdhe_precomp_p(value x6)
{
   uint8_t x7 = Uint8_val(x6);
   uint64_t* x10 = Hacl_FFDHE_new_ffdhe_precomp_p(x7);
   return CTYPES_FROM_PTR(x10);
}
value _3_Hacl_FFDHE_ffdhe_secret_to_public_precomp(value x14, value x13,
                                                   value x12, value x11)
{
   uint8_t x15 = Uint8_val(x14);
   uint64_t* x18 = CTYPES_ADDR_OF_FATPTR(x13);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   unsigned char* x20 = CTYPES_PTR_OF_OCAML_BYTES(x11);
   Hacl_FFDHE_ffdhe_secret_to_public_precomp(x15, x18, x19, x20);
   return Val_unit;
}
value _4_Hacl_FFDHE_ffdhe_secret_to_public(value x24, value x23, value x22)
{
   uint8_t x25 = Uint8_val(x24);
   unsigned char* x28 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   unsigned char* x29 = CTYPES_PTR_OF_OCAML_BYTES(x22);
   Hacl_FFDHE_ffdhe_secret_to_public(x25, x28, x29);
   return Val_unit;
}
value _5_Hacl_FFDHE_ffdhe_shared_secret_precomp(value x35, value x34,
                                                value x33, value x32,
                                                value x31)
{
   uint8_t x36 = Uint8_val(x35);
   uint64_t* x39 = CTYPES_ADDR_OF_FATPTR(x34);
   unsigned char* x40 = CTYPES_PTR_OF_OCAML_BYTES(x33);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x32);
   unsigned char* x42 = CTYPES_PTR_OF_OCAML_BYTES(x31);
   uint64_t x43 =
   Hacl_FFDHE_ffdhe_shared_secret_precomp(x36, x39, x40, x41, x42);
   return integers_copy_uint64(x43);
}
value _6_Hacl_FFDHE_ffdhe_shared_secret(value x47, value x46, value x45,
                                        value x44)
{
   uint8_t x48 = Uint8_val(x47);
   unsigned char* x51 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x45);
   unsigned char* x53 = CTYPES_PTR_OF_OCAML_BYTES(x44);
   uint64_t x54 = Hacl_FFDHE_ffdhe_shared_secret(x48, x51, x52, x53);
   return integers_copy_uint64(x54);
}
