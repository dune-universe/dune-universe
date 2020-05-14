
#include "EverCrypt_Chacha20Poly1305.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Chacha20Poly1305_aead_encrypt(value x8, value x7,
                                                 value x6, value x5,
                                                 value x4, value x3,
                                                 value x2, value x1)
{
   char* x9 = CTYPES_PTR_OF_OCAML_STRING(x8);
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x7);
   uint32_t x11 = Uint32_val(x6);
   char* x14 = CTYPES_PTR_OF_OCAML_STRING(x5);
   uint32_t x15 = Uint32_val(x4);
   char* x18 = CTYPES_PTR_OF_OCAML_STRING(x3);
   char* x19 = CTYPES_PTR_OF_OCAML_STRING(x2);
   char* x20 = CTYPES_PTR_OF_OCAML_STRING(x1);
   EverCrypt_Chacha20Poly1305_aead_encrypt(x9, x10, x11, x14, x15, x18, 
                                           x19, x20);
   return Val_unit;
}
value _1_EverCrypt_Chacha20Poly1305_aead_encrypt_byte8(value* argv, int argc)
{
   value x22 = argv[7];
   value x23 = argv[6];
   value x24 = argv[5];
   value x25 = argv[4];
   value x26 = argv[3];
   value x27 = argv[2];
   value x28 = argv[1];
   value x29 = argv[0];
   return
     _1_EverCrypt_Chacha20Poly1305_aead_encrypt(x29, x28, x27, x26, x25, 
                                                x24, x23, x22);
}
value _2_EverCrypt_Chacha20Poly1305_aead_decrypt(value x37, value x36,
                                                 value x35, value x34,
                                                 value x33, value x32,
                                                 value x31, value x30)
{
   char* x38 = CTYPES_PTR_OF_OCAML_STRING(x37);
   char* x39 = CTYPES_PTR_OF_OCAML_STRING(x36);
   uint32_t x40 = Uint32_val(x35);
   char* x43 = CTYPES_PTR_OF_OCAML_STRING(x34);
   uint32_t x44 = Uint32_val(x33);
   char* x47 = CTYPES_PTR_OF_OCAML_STRING(x32);
   char* x48 = CTYPES_PTR_OF_OCAML_STRING(x31);
   char* x49 = CTYPES_PTR_OF_OCAML_STRING(x30);
   uint32_t x50 =
   EverCrypt_Chacha20Poly1305_aead_decrypt(x38, x39, x40, x43, x44, x47, 
                                           x48, x49);
   return integers_copy_uint32(x50);
}
value _2_EverCrypt_Chacha20Poly1305_aead_decrypt_byte8(value* argv, int argc)
{
   value x51 = argv[7];
   value x52 = argv[6];
   value x53 = argv[5];
   value x54 = argv[4];
   value x55 = argv[3];
   value x56 = argv[2];
   value x57 = argv[1];
   value x58 = argv[0];
   return
     _2_EverCrypt_Chacha20Poly1305_aead_decrypt(x58, x57, x56, x55, x54, 
                                                x53, x52, x51);
}
