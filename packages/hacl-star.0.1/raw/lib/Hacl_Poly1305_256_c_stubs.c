
#include "Hacl_Poly1305_256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Poly1305_256_blocklen(value _)
{
   uint32_t* x1 = &Hacl_Poly1305_256_blocklen;
   return CTYPES_FROM_PTR(x1);
}
value _2_Hacl_Poly1305_256_poly1305_mac(value x5, value x4, value x3,
                                        value x2)
{
   char* x6 = CTYPES_PTR_OF_OCAML_STRING(x5);
   uint32_t x7 = Uint32_val(x4);
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x3);
   char* x11 = CTYPES_PTR_OF_OCAML_STRING(x2);
   Hacl_Poly1305_256_poly1305_mac(x6, x7, x10, x11);
   return Val_unit;
}
