
#include "Hacl_Curve25519_64_Slow.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Curve25519_64_Slow_scalarmult(value x3, value x2, value x1)
{
   char* x4 = CTYPES_PTR_OF_OCAML_STRING(x3);
   char* x5 = CTYPES_PTR_OF_OCAML_STRING(x2);
   char* x6 = CTYPES_PTR_OF_OCAML_STRING(x1);
   Hacl_Curve25519_64_Slow_scalarmult(x4, x5, x6);
   return Val_unit;
}
value _2_Hacl_Curve25519_64_Slow_secret_to_public(value x9, value x8)
{
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x9);
   char* x11 = CTYPES_PTR_OF_OCAML_STRING(x8);
   Hacl_Curve25519_64_Slow_secret_to_public(x10, x11);
   return Val_unit;
}
value _3_Hacl_Curve25519_64_Slow_ecdh(value x15, value x14, value x13)
{
   char* x16 = CTYPES_PTR_OF_OCAML_STRING(x15);
   char* x17 = CTYPES_PTR_OF_OCAML_STRING(x14);
   char* x18 = CTYPES_PTR_OF_OCAML_STRING(x13);
   _Bool x19 = Hacl_Curve25519_64_Slow_ecdh(x16, x17, x18);
   return Val_bool(x19);
}
