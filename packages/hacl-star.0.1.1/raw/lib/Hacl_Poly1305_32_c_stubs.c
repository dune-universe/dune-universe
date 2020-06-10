
#include "Hacl_Poly1305_32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Poly1305_32_blocklen(value _)
{
   uint32_t* x1 = &Hacl_Poly1305_32_blocklen;
   return CTYPES_FROM_PTR(x1);
}
value _2_Hacl_Poly1305_32_poly1305_init(value x3, value x2)
{
   uint64_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   unsigned char* x5 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   Hacl_Poly1305_32_poly1305_init(x4, x5);
   return Val_unit;
}
value _3_Hacl_Poly1305_32_poly1305_update1(value x8, value x7)
{
   uint64_t* x9 = CTYPES_ADDR_OF_FATPTR(x8);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   Hacl_Poly1305_32_poly1305_update1(x9, x10);
   return Val_unit;
}
value _4_Hacl_Poly1305_32_poly1305_update(value x14, value x13, value x12)
{
   uint64_t* x15 = CTYPES_ADDR_OF_FATPTR(x14);
   uint32_t x16 = Uint32_val(x13);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   Hacl_Poly1305_32_poly1305_update(x15, x16, x19);
   return Val_unit;
}
value _5_Hacl_Poly1305_32_poly1305_finish(value x23, value x22, value x21)
{
   unsigned char* x24 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   unsigned char* x25 = CTYPES_PTR_OF_OCAML_BYTES(x22);
   uint64_t* x26 = CTYPES_ADDR_OF_FATPTR(x21);
   Hacl_Poly1305_32_poly1305_finish(x24, x25, x26);
   return Val_unit;
}
value _6_Hacl_Poly1305_32_poly1305_mac(value x31, value x30, value x29,
                                       value x28)
{
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x31);
   uint32_t x33 = Uint32_val(x30);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   Hacl_Poly1305_32_poly1305_mac(x32, x33, x36, x37);
   return Val_unit;
}
