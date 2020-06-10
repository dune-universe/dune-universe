
#include "Hacl_Poly1305_128.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Poly1305_128_blocklen(value _)
{
   uint32_t* x1 = &Hacl_Poly1305_128_blocklen;
   return CTYPES_FROM_PTR(x1);
}
value _2_Hacl_Poly1305_128_poly1305_mac(value x5, value x4, value x3,
                                        value x2)
{
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x7 = Uint32_val(x4);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   Hacl_Poly1305_128_poly1305_mac(x6, x7, x10, x11);
   return Val_unit;
}
