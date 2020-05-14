
#include "Hacl_Curve25519_51.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_Curve25519_Field51_fadd(value x3, value x2, value x1)
{
   uint64_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   uint64_t* x5 = CTYPES_ADDR_OF_FATPTR(x2);
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x1);
   Hacl_Impl_Curve25519_Field51_fadd(x4, x5, x6);
   return Val_unit;
}
value _2_Hacl_Impl_Curve25519_Field51_fsub(value x10, value x9, value x8)
{
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   uint64_t* x12 = CTYPES_ADDR_OF_FATPTR(x9);
   uint64_t* x13 = CTYPES_ADDR_OF_FATPTR(x8);
   Hacl_Impl_Curve25519_Field51_fsub(x11, x12, x13);
   return Val_unit;
}
value _3_Hacl_Impl_Curve25519_Field51_fmul1(value x17, value x16, value x15)
{
   uint64_t* x18 = CTYPES_ADDR_OF_FATPTR(x17);
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t x20 = Uint64_val(x15);
   Hacl_Impl_Curve25519_Field51_fmul1(x18, x19, x20);
   return Val_unit;
}
value _4_Hacl_Curve25519_51_scalarmult(value x26, value x25, value x24)
{
   char* x27 = CTYPES_PTR_OF_OCAML_STRING(x26);
   char* x28 = CTYPES_PTR_OF_OCAML_STRING(x25);
   char* x29 = CTYPES_PTR_OF_OCAML_STRING(x24);
   Hacl_Curve25519_51_scalarmult(x27, x28, x29);
   return Val_unit;
}
value _5_Hacl_Curve25519_51_secret_to_public(value x32, value x31)
{
   char* x33 = CTYPES_PTR_OF_OCAML_STRING(x32);
   char* x34 = CTYPES_PTR_OF_OCAML_STRING(x31);
   Hacl_Curve25519_51_secret_to_public(x33, x34);
   return Val_unit;
}
value _6_Hacl_Curve25519_51_ecdh(value x38, value x37, value x36)
{
   char* x39 = CTYPES_PTR_OF_OCAML_STRING(x38);
   char* x40 = CTYPES_PTR_OF_OCAML_STRING(x37);
   char* x41 = CTYPES_PTR_OF_OCAML_STRING(x36);
   _Bool x42 = Hacl_Curve25519_51_ecdh(x39, x40, x41);
   return Val_bool(x42);
}
