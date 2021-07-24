
#include "Hacl_HKDF_Blake2s_128.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128(value x6, value x5,
                                                  value x4, value x3,
                                                  value x2, value x1)
{
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x9 = Uint32_val(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x13 = Uint32_val(x2);
   uint32_t x16 = Uint32_val(x1);
   Hacl_HKDF_Blake2s_128_expand_blake2s_128(x7, x8, x9, x12, x13, x16);
   return Val_unit;
}
value _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128_byte6(value* argv,
                                                        int argc)
{
   value x20 = argv[5];
   value x21 = argv[4];
   value x22 = argv[3];
   value x23 = argv[2];
   value x24 = argv[1];
   value x25 = argv[0];
   return
     _1_Hacl_HKDF_Blake2s_128_expand_blake2s_128(x25, x24, x23, x22, 
                                                 x21, x20);
}
value _2_Hacl_HKDF_Blake2s_128_extract_blake2s_128(value x30, value x29,
                                                   value x28, value x27,
                                                   value x26)
{
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x30);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   uint32_t x33 = Uint32_val(x28);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint32_t x37 = Uint32_val(x26);
   Hacl_HKDF_Blake2s_128_extract_blake2s_128(x31, x32, x33, x36, x37);
   return Val_unit;
}
