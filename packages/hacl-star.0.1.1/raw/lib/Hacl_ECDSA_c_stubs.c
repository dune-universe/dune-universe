
#include "Hacl_ECDSA.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_P256_DH_ecp256dh_i(value x2, value x1)
{
   unsigned char* x3 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   unsigned char* x4 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   uint64_t x5 = Hacl_Impl_P256_DH_ecp256dh_i(x3, x4);
   return integers_copy_uint64(x5);
}
value _2_Hacl_Impl_P256_DH_ecp256dh_r(value x8, value x7, value x6)
{
   unsigned char* x9 = CTYPES_PTR_OF_OCAML_BYTES(x8);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   uint64_t x12 = Hacl_Impl_P256_DH_ecp256dh_r(x9, x10, x11);
   return integers_copy_uint64(x12);
}
value _3_Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign(value x17, value x16,
                                              value x15, value x14,
                                              value x13)
{
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x19 = Uint32_val(x16);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   unsigned char* x24 = CTYPES_PTR_OF_OCAML_BYTES(x13);
   uint64_t x25 =
   Hacl_Impl_ECDSA_ecdsa_p256_sha2_sign(x18, x19, x22, x23, x24);
   return integers_copy_uint64(x25);
}
value _4_Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify(value x30, value x29,
                                                value x28, value x27,
                                                value x26)
{
   uint32_t x31 = Uint32_val(x30);
   unsigned char* x34 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x26);
   _Bool x38 =
   Hacl_Impl_ECDSA_ecdsa_p256_sha2_verify(x31, x34, x35, x36, x37);
   return Val_bool(x38);
}
