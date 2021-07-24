
#include "EverCrypt_Cipher.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_Cipher_chacha20(value x6, value x5, value x4, value x3,
                                   value x2, value x1)
{
   uint32_t x7 = Uint32_val(x6);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   unsigned char* x13 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x14 = Uint32_val(x1);
   EverCrypt_Cipher_chacha20(x7, x10, x11, x12, x13, x14);
   return Val_unit;
}
value _1_EverCrypt_Cipher_chacha20_byte6(value* argv, int argc)
{
   value x18 = argv[5];
   value x19 = argv[4];
   value x20 = argv[3];
   value x21 = argv[2];
   value x22 = argv[1];
   value x23 = argv[0];
   return _1_EverCrypt_Cipher_chacha20(x23, x22, x21, x20, x19, x18);
}
