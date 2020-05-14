
#include "Hacl_Frodo_KEM.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Frodo_KEM_crypto_kem_keypair(value x2, value x1)
{
   char* x3 = CTYPES_PTR_OF_OCAML_STRING(x2);
   char* x4 = CTYPES_PTR_OF_OCAML_STRING(x1);
   uint32_t x5 = Hacl_Frodo_KEM_crypto_kem_keypair(x3, x4);
   return integers_copy_uint32(x5);
}
value _2_Hacl_Frodo_KEM_crypto_kem_enc(value x8, value x7, value x6)
{
   char* x9 = CTYPES_PTR_OF_OCAML_STRING(x8);
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x7);
   char* x11 = CTYPES_PTR_OF_OCAML_STRING(x6);
   uint32_t x12 = Hacl_Frodo_KEM_crypto_kem_enc(x9, x10, x11);
   return integers_copy_uint32(x12);
}
value _3_Hacl_Frodo_KEM_crypto_kem_dec(value x15, value x14, value x13)
{
   char* x16 = CTYPES_PTR_OF_OCAML_STRING(x15);
   char* x17 = CTYPES_PTR_OF_OCAML_STRING(x14);
   char* x18 = CTYPES_PTR_OF_OCAML_STRING(x13);
   uint32_t x19 = Hacl_Frodo_KEM_crypto_kem_dec(x16, x17, x18);
   return integers_copy_uint32(x19);
}
