
#include "Hacl_Blake2s_128.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Blake2s_128_blake2s(value x6, value x5, value x4, value x3,
                                  value x2, value x1)
{
   uint32_t x7 = Uint32_val(x6);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x11 = Uint32_val(x4);
   unsigned char* x14 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x15 = Uint32_val(x2);
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   Hacl_Blake2s_128_blake2s(x7, x10, x11, x14, x15, x18);
   return Val_unit;
}
value _1_Hacl_Blake2s_128_blake2s_byte6(value* argv, int argc)
{
   value x20 = argv[5];
   value x21 = argv[4];
   value x22 = argv[3];
   value x23 = argv[2];
   value x24 = argv[1];
   value x25 = argv[0];
   return _1_Hacl_Blake2s_128_blake2s(x25, x24, x23, x22, x21, x20);
}
