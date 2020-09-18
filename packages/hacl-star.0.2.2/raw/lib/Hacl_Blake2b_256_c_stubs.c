
#include "Hacl_Blake2b_256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Hash_Blake2b_256_hash_blake2b_256(value x3, value x2, value x1)
{
   unsigned char* x4 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x5 = Uint32_val(x2);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   Hacl_Hash_Blake2b_256_hash_blake2b_256(x4, x5, x8);
   return Val_unit;
}
value _2_Hacl_Blake2b_256_blake2b(value x15, value x14, value x13, value x12,
                                  value x11, value x10)
{
   uint32_t x16 = Uint32_val(x15);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   uint32_t x20 = Uint32_val(x13);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   uint32_t x24 = Uint32_val(x11);
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   Hacl_Blake2b_256_blake2b(x16, x19, x20, x23, x24, x27);
   return Val_unit;
}
value _2_Hacl_Blake2b_256_blake2b_byte6(value* argv, int argc)
{
   value x29 = argv[5];
   value x30 = argv[4];
   value x31 = argv[3];
   value x32 = argv[2];
   value x33 = argv[1];
   value x34 = argv[0];
   return _2_Hacl_Blake2b_256_blake2b(x34, x33, x32, x31, x30, x29);
}
