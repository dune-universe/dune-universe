
#include "Hacl_Chacha20.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_Chacha20_chacha20_init(value x4, value x3, value x2,
                                          value x1)
{
   uint32_t* x5 = CTYPES_ADDR_OF_FATPTR(x4);
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x8 = Uint32_val(x1);
   Hacl_Impl_Chacha20_chacha20_init(x5, x6, x7, x8);
   return Val_unit;
}
value _2_Hacl_Impl_Chacha20_chacha20_encrypt_block(value x15, value x14,
                                                   value x13, value x12)
{
   uint32_t* x16 = CTYPES_ADDR_OF_FATPTR(x15);
   unsigned char* x17 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   uint32_t x18 = Uint32_val(x13);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x12);
   Hacl_Impl_Chacha20_chacha20_encrypt_block(x16, x17, x18, x21);
   return Val_unit;
}
value _3_Hacl_Impl_Chacha20_chacha20_update(value x26, value x25, value x24,
                                            value x23)
{
   uint32_t* x27 = CTYPES_ADDR_OF_FATPTR(x26);
   uint32_t x28 = Uint32_val(x25);
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x24);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x23);
   Hacl_Impl_Chacha20_chacha20_update(x27, x28, x31, x32);
   return Val_unit;
}
value _4_Hacl_Chacha20_chacha20_encrypt(value x39, value x38, value x37,
                                        value x36, value x35, value x34)
{
   uint32_t x40 = Uint32_val(x39);
   unsigned char* x43 = CTYPES_PTR_OF_OCAML_BYTES(x38);
   unsigned char* x44 = CTYPES_PTR_OF_OCAML_BYTES(x37);
   unsigned char* x45 = CTYPES_PTR_OF_OCAML_BYTES(x36);
   unsigned char* x46 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   uint32_t x47 = Uint32_val(x34);
   Hacl_Chacha20_chacha20_encrypt(x40, x43, x44, x45, x46, x47);
   return Val_unit;
}
value _4_Hacl_Chacha20_chacha20_encrypt_byte6(value* argv, int argc)
{
   value x51 = argv[5];
   value x52 = argv[4];
   value x53 = argv[3];
   value x54 = argv[2];
   value x55 = argv[1];
   value x56 = argv[0];
   return _4_Hacl_Chacha20_chacha20_encrypt(x56, x55, x54, x53, x52, x51);
}
value _5_Hacl_Chacha20_chacha20_decrypt(value x62, value x61, value x60,
                                        value x59, value x58, value x57)
{
   uint32_t x63 = Uint32_val(x62);
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x61);
   unsigned char* x67 = CTYPES_PTR_OF_OCAML_BYTES(x60);
   unsigned char* x68 = CTYPES_PTR_OF_OCAML_BYTES(x59);
   unsigned char* x69 = CTYPES_PTR_OF_OCAML_BYTES(x58);
   uint32_t x70 = Uint32_val(x57);
   Hacl_Chacha20_chacha20_decrypt(x63, x66, x67, x68, x69, x70);
   return Val_unit;
}
value _5_Hacl_Chacha20_chacha20_decrypt_byte6(value* argv, int argc)
{
   value x74 = argv[5];
   value x75 = argv[4];
   value x76 = argv[3];
   value x77 = argv[2];
   value x78 = argv[1];
   value x79 = argv[0];
   return _5_Hacl_Chacha20_chacha20_decrypt(x79, x78, x77, x76, x75, x74);
}
