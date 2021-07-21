
#include "Hacl_Blake2s_32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Blake2s_32_blake2s_init(value x5, value x4, value x3, value x2,
                                      value x1)
{
   uint32_t* x6 = CTYPES_ADDR_OF_FATPTR(x5);
   uint32_t* x7 = CTYPES_ADDR_OF_FATPTR(x4);
   uint32_t x8 = Uint32_val(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x12 = Uint32_val(x1);
   Hacl_Blake2s_32_blake2s_init(x6, x7, x8, x11, x12);
   return Val_unit;
}
value _2_Hacl_Blake2s_32_blake2s_update_multi(value x21, value x20,
                                              value x19, value x18,
                                              value x17, value x16)
{
   uint32_t x22 = Uint32_val(x21);
   uint32_t* x25 = CTYPES_ADDR_OF_FATPTR(x20);
   uint32_t* x26 = CTYPES_ADDR_OF_FATPTR(x19);
   uint64_t x27 = Uint64_val(x18);
   unsigned char* x30 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x31 = Uint32_val(x16);
   Hacl_Blake2s_32_blake2s_update_multi(x22, x25, x26, x27, x30, x31);
   return Val_unit;
}
value _2_Hacl_Blake2s_32_blake2s_update_multi_byte6(value* argv, int argc)
{
   value x35 = argv[5];
   value x36 = argv[4];
   value x37 = argv[3];
   value x38 = argv[2];
   value x39 = argv[1];
   value x40 = argv[0];
   return
     _2_Hacl_Blake2s_32_blake2s_update_multi(x40, x39, x38, x37, x36, x35);
}
value _3_Hacl_Blake2s_32_blake2s_update_last(value x46, value x45, value x44,
                                             value x43, value x42, value x41)
{
   uint32_t x47 = Uint32_val(x46);
   uint32_t* x50 = CTYPES_ADDR_OF_FATPTR(x45);
   uint32_t* x51 = CTYPES_ADDR_OF_FATPTR(x44);
   uint64_t x52 = Uint64_val(x43);
   uint32_t x55 = Uint32_val(x42);
   unsigned char* x58 = CTYPES_PTR_OF_OCAML_BYTES(x41);
   Hacl_Blake2s_32_blake2s_update_last(x47, x50, x51, x52, x55, x58);
   return Val_unit;
}
value _3_Hacl_Blake2s_32_blake2s_update_last_byte6(value* argv, int argc)
{
   value x60 = argv[5];
   value x61 = argv[4];
   value x62 = argv[3];
   value x63 = argv[2];
   value x64 = argv[1];
   value x65 = argv[0];
   return
     _3_Hacl_Blake2s_32_blake2s_update_last(x65, x64, x63, x62, x61, x60);
}
value _4_Hacl_Blake2s_32_blake2s_finish(value x68, value x67, value x66)
{
   uint32_t x69 = Uint32_val(x68);
   unsigned char* x72 = CTYPES_PTR_OF_OCAML_BYTES(x67);
   uint32_t* x73 = CTYPES_ADDR_OF_FATPTR(x66);
   Hacl_Blake2s_32_blake2s_finish(x69, x72, x73);
   return Val_unit;
}
value _5_Hacl_Blake2s_32_blake2s(value x80, value x79, value x78, value x77,
                                 value x76, value x75)
{
   uint32_t x81 = Uint32_val(x80);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x79);
   uint32_t x85 = Uint32_val(x78);
   unsigned char* x88 = CTYPES_PTR_OF_OCAML_BYTES(x77);
   uint32_t x89 = Uint32_val(x76);
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x75);
   Hacl_Blake2s_32_blake2s(x81, x84, x85, x88, x89, x92);
   return Val_unit;
}
value _5_Hacl_Blake2s_32_blake2s_byte6(value* argv, int argc)
{
   value x94 = argv[5];
   value x95 = argv[4];
   value x96 = argv[3];
   value x97 = argv[2];
   value x98 = argv[1];
   value x99 = argv[0];
   return _5_Hacl_Blake2s_32_blake2s(x99, x98, x97, x96, x95, x94);
}
