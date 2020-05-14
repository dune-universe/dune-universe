
#include "Hacl_Ed25519.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Ed25519_sign(value x4, value x3, value x2, value x1)
{
   char* x5 = CTYPES_PTR_OF_OCAML_STRING(x4);
   char* x6 = CTYPES_PTR_OF_OCAML_STRING(x3);
   uint32_t x7 = Uint32_val(x2);
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x1);
   Hacl_Ed25519_sign(x5, x6, x7, x10);
   return Val_unit;
}
value _2_Hacl_Ed25519_verify(value x15, value x14, value x13, value x12)
{
   char* x16 = CTYPES_PTR_OF_OCAML_STRING(x15);
   uint32_t x17 = Uint32_val(x14);
   char* x20 = CTYPES_PTR_OF_OCAML_STRING(x13);
   char* x21 = CTYPES_PTR_OF_OCAML_STRING(x12);
   _Bool x22 = Hacl_Ed25519_verify(x16, x17, x20, x21);
   return Val_bool(x22);
}
value _3_Hacl_Ed25519_secret_to_public(value x24, value x23)
{
   char* x25 = CTYPES_PTR_OF_OCAML_STRING(x24);
   char* x26 = CTYPES_PTR_OF_OCAML_STRING(x23);
   Hacl_Ed25519_secret_to_public(x25, x26);
   return Val_unit;
}
value _4_Hacl_Ed25519_expand_keys(value x29, value x28)
{
   char* x30 = CTYPES_PTR_OF_OCAML_STRING(x29);
   char* x31 = CTYPES_PTR_OF_OCAML_STRING(x28);
   Hacl_Ed25519_expand_keys(x30, x31);
   return Val_unit;
}
value _5_Hacl_Ed25519_sign_expanded(value x36, value x35, value x34,
                                    value x33)
{
   char* x37 = CTYPES_PTR_OF_OCAML_STRING(x36);
   char* x38 = CTYPES_PTR_OF_OCAML_STRING(x35);
   uint32_t x39 = Uint32_val(x34);
   char* x42 = CTYPES_PTR_OF_OCAML_STRING(x33);
   Hacl_Ed25519_sign_expanded(x37, x38, x39, x42);
   return Val_unit;
}
