
#include "EverCrypt_CTR.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_CTR_xor8(value x2, value x1)
{
   uint8_t x3 = Uint8_val(x2);
   uint8_t x6 = Uint8_val(x1);
   uint8_t x9 = EverCrypt_CTR_xor8(x3, x6);
   return Integers_val_uint8(x9);
}
value _2_EverCrypt_CTR_alg_of_state(value x10)
{
   struct EverCrypt_CTR_state_s_s* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   Spec_Agile_Cipher_cipher_alg x12 = EverCrypt_CTR_alg_of_state(x11);
   return Integers_val_uint8(x12);
}
value _3_EverCrypt_CTR_create_in(value x18, value x17, value x16, value x15,
                                 value x14, value x13)
{
   uint8_t x19 = Uint8_val(x18);
   struct EverCrypt_CTR_state_s_s** x22 = CTYPES_ADDR_OF_FATPTR(x17);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x16);
   unsigned char* x24 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   uint32_t x25 = Uint32_val(x14);
   uint32_t x28 = Uint32_val(x13);
   EverCrypt_Error_error_code x31 =
   EverCrypt_CTR_create_in(x19, x22, x23, x24, x25, x28);
   return Integers_val_uint8(x31);
}
value _3_EverCrypt_CTR_create_in_byte6(value* argv, int argc)
{
   value x32 = argv[5];
   value x33 = argv[4];
   value x34 = argv[3];
   value x35 = argv[2];
   value x36 = argv[1];
   value x37 = argv[0];
   return _3_EverCrypt_CTR_create_in(x37, x36, x35, x34, x33, x32);
}
value _4_EverCrypt_CTR_init(value x42, value x41, value x40, value x39,
                            value x38)
{
   struct EverCrypt_CTR_state_s_s* x43 = CTYPES_ADDR_OF_FATPTR(x42);
   unsigned char* x44 = CTYPES_PTR_OF_OCAML_BYTES(x41);
   unsigned char* x45 = CTYPES_PTR_OF_OCAML_BYTES(x40);
   uint32_t x46 = Uint32_val(x39);
   uint32_t x49 = Uint32_val(x38);
   EverCrypt_CTR_init(x43, x44, x45, x46, x49);
   return Val_unit;
}
value _5_EverCrypt_CTR_update_block(value x55, value x54, value x53)
{
   struct EverCrypt_CTR_state_s_s* x56 = CTYPES_ADDR_OF_FATPTR(x55);
   unsigned char* x57 = CTYPES_PTR_OF_OCAML_BYTES(x54);
   unsigned char* x58 = CTYPES_PTR_OF_OCAML_BYTES(x53);
   EverCrypt_CTR_update_block(x56, x57, x58);
   return Val_unit;
}
value _6_EverCrypt_CTR_free(value x60)
{
   struct EverCrypt_CTR_state_s_s* x61 = CTYPES_ADDR_OF_FATPTR(x60);
   EverCrypt_CTR_free(x61);
   return Val_unit;
}
