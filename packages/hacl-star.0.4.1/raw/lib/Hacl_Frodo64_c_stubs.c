
#include "Hacl_Frodo64.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Frodo64_crypto_bytes(value _)
{
   uint32_t* x1 = &Hacl_Frodo64_crypto_bytes;
   return CTYPES_FROM_PTR(x1);
}
value _2_Hacl_Frodo64_crypto_publickeybytes(value _)
{
   uint32_t* x2 = &Hacl_Frodo64_crypto_publickeybytes;
   return CTYPES_FROM_PTR(x2);
}
value _3_Hacl_Frodo64_crypto_secretkeybytes(value _)
{
   uint32_t* x3 = &Hacl_Frodo64_crypto_secretkeybytes;
   return CTYPES_FROM_PTR(x3);
}
value _4_Hacl_Frodo64_crypto_ciphertextbytes(value _)
{
   uint32_t* x4 = &Hacl_Frodo64_crypto_ciphertextbytes;
   return CTYPES_FROM_PTR(x4);
}
value _5_Hacl_Frodo64_crypto_kem_keypair(value x6, value x5)
{
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x9 = Hacl_Frodo64_crypto_kem_keypair(x7, x8);
   return integers_copy_uint32(x9);
}
value _6_Hacl_Frodo64_crypto_kem_enc(value x12, value x11, value x10)
{
   unsigned char* x13 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   unsigned char* x14 = CTYPES_PTR_OF_OCAML_BYTES(x11);
   unsigned char* x15 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   uint32_t x16 = Hacl_Frodo64_crypto_kem_enc(x13, x14, x15);
   return integers_copy_uint32(x16);
}
value _7_Hacl_Frodo64_crypto_kem_dec(value x19, value x18, value x17)
{
   unsigned char* x20 = CTYPES_PTR_OF_OCAML_BYTES(x19);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x23 = Hacl_Frodo64_crypto_kem_dec(x20, x21, x22);
   return integers_copy_uint32(x23);
}
