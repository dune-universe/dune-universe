
#include "EverCrypt_Curve25519.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Curve25519_secret_to_public(value x2, value x1)
{
   unsigned char* x3 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   unsigned char* x4 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   EverCrypt_Curve25519_secret_to_public(x3, x4);
   return Val_unit;
}
value _2_EverCrypt_Curve25519_scalarmult(value x8, value x7, value x6)
{
   unsigned char* x9 = CTYPES_PTR_OF_OCAML_BYTES(x8);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   EverCrypt_Curve25519_scalarmult(x9, x10, x11);
   return Val_unit;
}
value _3_EverCrypt_Curve25519_ecdh(value x15, value x14, value x13)
{
   unsigned char* x16 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   unsigned char* x17 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x13);
   _Bool x19 = EverCrypt_Curve25519_ecdh(x16, x17, x18);
   return Val_bool(x19);
}
