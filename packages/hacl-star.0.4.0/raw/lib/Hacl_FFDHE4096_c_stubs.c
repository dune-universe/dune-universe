
#include "Hacl_FFDHE4096.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_FFDHE4096_new_ffdhe_precomp_p(value x1)
{
   uint64_t* x2 = Hacl_FFDHE4096_new_ffdhe_precomp_p();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_FFDHE4096_ffdhe_secret_to_public_precomp(value x5, value x4,
                                                       value x3)
{
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   Hacl_FFDHE4096_ffdhe_secret_to_public_precomp(x6, x7, x8);
   return Val_unit;
}
value _3_Hacl_FFDHE4096_ffdhe_secret_to_public(value x11, value x10)
{
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x11);
   unsigned char* x13 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   Hacl_FFDHE4096_ffdhe_secret_to_public(x12, x13);
   return Val_unit;
}
value _4_Hacl_FFDHE4096_ffdhe_shared_secret_precomp(value x18, value x17,
                                                    value x16, value x15)
{
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x18);
   unsigned char* x20 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x16);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   uint64_t x23 =
   Hacl_FFDHE4096_ffdhe_shared_secret_precomp(x19, x20, x21, x22);
   return integers_copy_uint64(x23);
}
value _5_Hacl_FFDHE4096_ffdhe_shared_secret(value x26, value x25, value x24)
{
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x26);
   unsigned char* x28 = CTYPES_PTR_OF_OCAML_BYTES(x25);
   unsigned char* x29 = CTYPES_PTR_OF_OCAML_BYTES(x24);
   uint64_t x30 = Hacl_FFDHE4096_ffdhe_shared_secret(x27, x28, x29);
   return integers_copy_uint64(x30);
}
