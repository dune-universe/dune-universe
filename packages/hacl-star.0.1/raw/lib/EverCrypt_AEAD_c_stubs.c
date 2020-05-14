
#include "EverCrypt_AEAD.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_AEAD_alg_of_state(value x1)
{
   struct EverCrypt_AEAD_state_s_s* x2 = CTYPES_ADDR_OF_FATPTR(x1);
   Spec_Agile_AEAD_alg x3 = EverCrypt_AEAD_alg_of_state(x2);
   return Integers_val_uint8(x3);
}
value _2_EverCrypt_AEAD_create_in(value x6, value x5, value x4)
{
   uint8_t x7 = Uint8_val(x6);
   struct EverCrypt_AEAD_state_s_s** x10 = CTYPES_ADDR_OF_FATPTR(x5);
   char* x11 = CTYPES_PTR_OF_OCAML_STRING(x4);
   EverCrypt_Error_error_code x12 = EverCrypt_AEAD_create_in(x7, x10, x11);
   return Integers_val_uint8(x12);
}
value _3_EverCrypt_AEAD_encrypt(value x21, value x20, value x19, value x18,
                                value x17, value x16, value x15, value x14,
                                value x13)
{
   struct EverCrypt_AEAD_state_s_s* x22 = CTYPES_ADDR_OF_FATPTR(x21);
   char* x23 = CTYPES_PTR_OF_OCAML_STRING(x20);
   uint32_t x24 = Uint32_val(x19);
   char* x27 = CTYPES_PTR_OF_OCAML_STRING(x18);
   uint32_t x28 = Uint32_val(x17);
   char* x31 = CTYPES_PTR_OF_OCAML_STRING(x16);
   uint32_t x32 = Uint32_val(x15);
   char* x35 = CTYPES_PTR_OF_OCAML_STRING(x14);
   char* x36 = CTYPES_PTR_OF_OCAML_STRING(x13);
   EverCrypt_Error_error_code x37 =
   EverCrypt_AEAD_encrypt(x22, x23, x24, x27, x28, x31, x32, x35, x36);
   return Integers_val_uint8(x37);
}
value _3_EverCrypt_AEAD_encrypt_byte9(value* argv, int argc)
{
   value x38 = argv[8];
   value x39 = argv[7];
   value x40 = argv[6];
   value x41 = argv[5];
   value x42 = argv[4];
   value x43 = argv[3];
   value x44 = argv[2];
   value x45 = argv[1];
   value x46 = argv[0];
   return
     _3_EverCrypt_AEAD_encrypt(x46, x45, x44, x43, x42, x41, x40, x39, x38);
}
value _4_EverCrypt_AEAD_decrypt(value x55, value x54, value x53, value x52,
                                value x51, value x50, value x49, value x48,
                                value x47)
{
   struct EverCrypt_AEAD_state_s_s* x56 = CTYPES_ADDR_OF_FATPTR(x55);
   char* x57 = CTYPES_PTR_OF_OCAML_STRING(x54);
   uint32_t x58 = Uint32_val(x53);
   char* x61 = CTYPES_PTR_OF_OCAML_STRING(x52);
   uint32_t x62 = Uint32_val(x51);
   char* x65 = CTYPES_PTR_OF_OCAML_STRING(x50);
   uint32_t x66 = Uint32_val(x49);
   char* x69 = CTYPES_PTR_OF_OCAML_STRING(x48);
   char* x70 = CTYPES_PTR_OF_OCAML_STRING(x47);
   EverCrypt_Error_error_code x71 =
   EverCrypt_AEAD_decrypt(x56, x57, x58, x61, x62, x65, x66, x69, x70);
   return Integers_val_uint8(x71);
}
value _4_EverCrypt_AEAD_decrypt_byte9(value* argv, int argc)
{
   value x72 = argv[8];
   value x73 = argv[7];
   value x74 = argv[6];
   value x75 = argv[5];
   value x76 = argv[4];
   value x77 = argv[3];
   value x78 = argv[2];
   value x79 = argv[1];
   value x80 = argv[0];
   return
     _4_EverCrypt_AEAD_decrypt(x80, x79, x78, x77, x76, x75, x74, x73, x72);
}
value _5_EverCrypt_AEAD_free(value x81)
{
   struct EverCrypt_AEAD_state_s_s* x82 = CTYPES_ADDR_OF_FATPTR(x81);
   EverCrypt_AEAD_free(x82);
   return Val_unit;
}
