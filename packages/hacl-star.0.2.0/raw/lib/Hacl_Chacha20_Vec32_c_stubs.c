
#include "Hacl_Chacha20_Vec32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32(value x6, value x5,
                                                 value x4, value x3,
                                                 value x2, value x1)
{
   uint32_t x7 = Uint32_val(x6);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   unsigned char* x13 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x14 = Uint32_val(x1);
   Hacl_Chacha20_Vec32_chacha20_encrypt_32(x7, x10, x11, x12, x13, x14);
   return Val_unit;
}
value _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32_byte6(value* argv, int argc)
{
   value x18 = argv[5];
   value x19 = argv[4];
   value x20 = argv[3];
   value x21 = argv[2];
   value x22 = argv[1];
   value x23 = argv[0];
   return
     _1_Hacl_Chacha20_Vec32_chacha20_encrypt_32(x23, x22, x21, x20, x19, x18);
}
value _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32(value x29, value x28,
                                                 value x27, value x26,
                                                 value x25, value x24)
{
   uint32_t x30 = Uint32_val(x29);
   unsigned char* x33 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   unsigned char* x34 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x26);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x25);
   uint32_t x37 = Uint32_val(x24);
   Hacl_Chacha20_Vec32_chacha20_decrypt_32(x30, x33, x34, x35, x36, x37);
   return Val_unit;
}
value _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32_byte6(value* argv, int argc)
{
   value x41 = argv[5];
   value x42 = argv[4];
   value x43 = argv[3];
   value x44 = argv[2];
   value x45 = argv[1];
   value x46 = argv[0];
   return
     _2_Hacl_Chacha20_Vec32_chacha20_decrypt_32(x46, x45, x44, x43, x42, x41);
}
