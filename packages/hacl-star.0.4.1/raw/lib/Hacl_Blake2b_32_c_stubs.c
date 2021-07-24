
#include "Hacl_Blake2b_32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Blake2b_32_blake2b_init(value x5, value x4, value x3, value x2,
                                      value x1)
{
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x5);
   uint64_t* x7 = CTYPES_ADDR_OF_FATPTR(x4);
   uint32_t x8 = Uint32_val(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x12 = Uint32_val(x1);
   Hacl_Blake2b_32_blake2b_init(x6, x7, x8, x11, x12);
   return Val_unit;
}
value _2_Hacl_Blake2b_32_blake2b_finish(value x18, value x17, value x16)
{
   uint32_t x19 = Uint32_val(x18);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint64_t* x23 = CTYPES_ADDR_OF_FATPTR(x16);
   Hacl_Blake2b_32_blake2b_finish(x19, x22, x23);
   return Val_unit;
}
value _3_Hacl_Blake2b_32_blake2b(value x30, value x29, value x28, value x27,
                                 value x26, value x25)
{
   uint32_t x31 = Uint32_val(x30);
   unsigned char* x34 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   uint32_t x35 = Uint32_val(x28);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint32_t x39 = Uint32_val(x26);
   unsigned char* x42 = CTYPES_PTR_OF_OCAML_BYTES(x25);
   Hacl_Blake2b_32_blake2b(x31, x34, x35, x38, x39, x42);
   return Val_unit;
}
value _3_Hacl_Blake2b_32_blake2b_byte6(value* argv, int argc)
{
   value x44 = argv[5];
   value x45 = argv[4];
   value x46 = argv[3];
   value x47 = argv[2];
   value x48 = argv[1];
   value x49 = argv[0];
   return _3_Hacl_Blake2b_32_blake2b(x49, x48, x47, x46, x45, x44);
}
