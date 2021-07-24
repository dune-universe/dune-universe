
#include "EverCrypt_HMAC.h"
#include "ctypes_cstubs_internals.h"
value _1_EverCrypt_HMAC_compute_sha1(value x5, value x4, value x3, value x2,
                                     value x1)
{
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x8 = Uint32_val(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x12 = Uint32_val(x1);
   EverCrypt_HMAC_compute_sha1(x6, x7, x8, x11, x12);
   return Val_unit;
}
value _2_EverCrypt_HMAC_compute_sha2_256(value x20, value x19, value x18,
                                         value x17, value x16)
{
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x19);
   uint32_t x23 = Uint32_val(x18);
   unsigned char* x26 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x27 = Uint32_val(x16);
   EverCrypt_HMAC_compute_sha2_256(x21, x22, x23, x26, x27);
   return Val_unit;
}
value _3_EverCrypt_HMAC_compute_sha2_384(value x35, value x34, value x33,
                                         value x32, value x31)
{
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x34);
   uint32_t x38 = Uint32_val(x33);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x32);
   uint32_t x42 = Uint32_val(x31);
   EverCrypt_HMAC_compute_sha2_384(x36, x37, x38, x41, x42);
   return Val_unit;
}
value _4_EverCrypt_HMAC_compute_sha2_512(value x50, value x49, value x48,
                                         value x47, value x46)
{
   unsigned char* x51 = CTYPES_PTR_OF_OCAML_BYTES(x50);
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x49);
   uint32_t x53 = Uint32_val(x48);
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   uint32_t x57 = Uint32_val(x46);
   EverCrypt_HMAC_compute_sha2_512(x51, x52, x53, x56, x57);
   return Val_unit;
}
value _5_EverCrypt_HMAC_compute_blake2s(value x65, value x64, value x63,
                                        value x62, value x61)
{
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x65);
   unsigned char* x67 = CTYPES_PTR_OF_OCAML_BYTES(x64);
   uint32_t x68 = Uint32_val(x63);
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x62);
   uint32_t x72 = Uint32_val(x61);
   EverCrypt_HMAC_compute_blake2s(x66, x67, x68, x71, x72);
   return Val_unit;
}
value _6_EverCrypt_HMAC_compute_blake2b(value x80, value x79, value x78,
                                        value x77, value x76)
{
   unsigned char* x81 = CTYPES_PTR_OF_OCAML_BYTES(x80);
   unsigned char* x82 = CTYPES_PTR_OF_OCAML_BYTES(x79);
   uint32_t x83 = Uint32_val(x78);
   unsigned char* x86 = CTYPES_PTR_OF_OCAML_BYTES(x77);
   uint32_t x87 = Uint32_val(x76);
   EverCrypt_HMAC_compute_blake2b(x81, x82, x83, x86, x87);
   return Val_unit;
}
value _7_EverCrypt_HMAC_is_supported_alg(value x91)
{
   uint8_t x92 = Uint8_val(x91);
   _Bool x95 = EverCrypt_HMAC_is_supported_alg(x92);
   return Val_bool(x95);
}
value _8_EverCrypt_HMAC_compute(value x101, value x100, value x99, value x98,
                                value x97, value x96)
{
   uint8_t x102 = Uint8_val(x101);
   unsigned char* x105 = CTYPES_PTR_OF_OCAML_BYTES(x100);
   unsigned char* x106 = CTYPES_PTR_OF_OCAML_BYTES(x99);
   uint32_t x107 = Uint32_val(x98);
   unsigned char* x110 = CTYPES_PTR_OF_OCAML_BYTES(x97);
   uint32_t x111 = Uint32_val(x96);
   EverCrypt_HMAC_compute(x102, x105, x106, x107, x110, x111);
   return Val_unit;
}
value _8_EverCrypt_HMAC_compute_byte6(value* argv, int argc)
{
   value x115 = argv[5];
   value x116 = argv[4];
   value x117 = argv[3];
   value x118 = argv[2];
   value x119 = argv[1];
   value x120 = argv[0];
   return _8_EverCrypt_HMAC_compute(x120, x119, x118, x117, x116, x115);
}
