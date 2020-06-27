
#include "EverCrypt_Ed25519.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Ed25519_sign(value x4, value x3, value x2, value x1)
{
   unsigned char* x5 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x7 = Uint32_val(x2);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   EverCrypt_Ed25519_sign(x5, x6, x7, x10);
   return Val_unit;
}
value _2_EverCrypt_Ed25519_verify(value x15, value x14, value x13, value x12)
{
   unsigned char* x16 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   uint32_t x17 = Uint32_val(x14);
   unsigned char* x20 = CTYPES_PTR_OF_OCAML_BYTES(x13);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   _Bool x22 = EverCrypt_Ed25519_verify(x16, x17, x20, x21);
   return Val_bool(x22);
}
value _3_EverCrypt_Ed25519_secret_to_public(value x24, value x23)
{
   unsigned char* x25 = CTYPES_PTR_OF_OCAML_BYTES(x24);
   unsigned char* x26 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   EverCrypt_Ed25519_secret_to_public(x25, x26);
   return Val_unit;
}
value _4_EverCrypt_Ed25519_expand_keys(value x29, value x28)
{
   unsigned char* x30 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   EverCrypt_Ed25519_expand_keys(x30, x31);
   return Val_unit;
}
value _5_EverCrypt_Ed25519_sign_expanded(value x36, value x35, value x34,
                                         value x33)
{
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x36);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   uint32_t x39 = Uint32_val(x34);
   unsigned char* x42 = CTYPES_PTR_OF_OCAML_BYTES(x33);
   EverCrypt_Ed25519_sign_expanded(x37, x38, x39, x42);
   return Val_unit;
}
