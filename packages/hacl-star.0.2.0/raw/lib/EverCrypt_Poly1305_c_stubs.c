
#include "EverCrypt_Poly1305.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Poly1305_poly1305(value x4, value x3, value x2, value x1)
{
   unsigned char* x5 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x7 = Uint32_val(x2);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   EverCrypt_Poly1305_poly1305(x5, x6, x7, x10);
   return Val_unit;
}
