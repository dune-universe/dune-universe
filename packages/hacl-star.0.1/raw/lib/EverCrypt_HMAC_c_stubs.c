
#include "EverCrypt_HMAC.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_HMAC_compute_sha1(value x5, value x4, value x3, value x2,
                                     value x1)
{
   char* x6 = CTYPES_PTR_OF_OCAML_STRING(x5);
   char* x7 = CTYPES_PTR_OF_OCAML_STRING(x4);
   uint32_t x8 = Uint32_val(x3);
   char* x11 = CTYPES_PTR_OF_OCAML_STRING(x2);
   uint32_t x12 = Uint32_val(x1);
   EverCrypt_HMAC_compute_sha1(x6, x7, x8, x11, x12);
   return Val_unit;
}
value _2_EverCrypt_HMAC_compute_sha2_256(value x20, value x19, value x18,
                                         value x17, value x16)
{
   char* x21 = CTYPES_PTR_OF_OCAML_STRING(x20);
   char* x22 = CTYPES_PTR_OF_OCAML_STRING(x19);
   uint32_t x23 = Uint32_val(x18);
   char* x26 = CTYPES_PTR_OF_OCAML_STRING(x17);
   uint32_t x27 = Uint32_val(x16);
   EverCrypt_HMAC_compute_sha2_256(x21, x22, x23, x26, x27);
   return Val_unit;
}
value _3_EverCrypt_HMAC_compute_sha2_384(value x35, value x34, value x33,
                                         value x32, value x31)
{
   char* x36 = CTYPES_PTR_OF_OCAML_STRING(x35);
   char* x37 = CTYPES_PTR_OF_OCAML_STRING(x34);
   uint32_t x38 = Uint32_val(x33);
   char* x41 = CTYPES_PTR_OF_OCAML_STRING(x32);
   uint32_t x42 = Uint32_val(x31);
   EverCrypt_HMAC_compute_sha2_384(x36, x37, x38, x41, x42);
   return Val_unit;
}
value _4_EverCrypt_HMAC_compute_sha2_512(value x50, value x49, value x48,
                                         value x47, value x46)
{
   char* x51 = CTYPES_PTR_OF_OCAML_STRING(x50);
   char* x52 = CTYPES_PTR_OF_OCAML_STRING(x49);
   uint32_t x53 = Uint32_val(x48);
   char* x56 = CTYPES_PTR_OF_OCAML_STRING(x47);
   uint32_t x57 = Uint32_val(x46);
   EverCrypt_HMAC_compute_sha2_512(x51, x52, x53, x56, x57);
   return Val_unit;
}
value _5_EverCrypt_HMAC_is_supported_alg(value x61)
{
   uint8_t x62 = Uint8_val(x61);
   _Bool x65 = EverCrypt_HMAC_is_supported_alg(x62);
   return Val_bool(x65);
}
value _6_EverCrypt_HMAC_compute(value x71, value x70, value x69, value x68,
                                value x67, value x66)
{
   uint8_t x72 = Uint8_val(x71);
   char* x75 = CTYPES_PTR_OF_OCAML_STRING(x70);
   char* x76 = CTYPES_PTR_OF_OCAML_STRING(x69);
   uint32_t x77 = Uint32_val(x68);
   char* x80 = CTYPES_PTR_OF_OCAML_STRING(x67);
   uint32_t x81 = Uint32_val(x66);
   EverCrypt_HMAC_compute(x72, x75, x76, x77, x80, x81);
   return Val_unit;
}
value _6_EverCrypt_HMAC_compute_byte6(value* argv, int argc)
{
   value x85 = argv[5];
   value x86 = argv[4];
   value x87 = argv[3];
   value x88 = argv[2];
   value x89 = argv[1];
   value x90 = argv[0];
   return _6_EverCrypt_HMAC_compute(x90, x89, x88, x87, x86, x85);
}
