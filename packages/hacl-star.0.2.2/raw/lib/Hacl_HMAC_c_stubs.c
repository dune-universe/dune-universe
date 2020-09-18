
#include "Hacl_HMAC.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HMAC_legacy_compute_sha1(value x5, value x4, value x3,
                                       value x2, value x1)
{
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x8 = Uint32_val(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x12 = Uint32_val(x1);
   Hacl_HMAC_legacy_compute_sha1(x6, x7, x8, x11, x12);
   return Val_unit;
}
value _2_Hacl_HMAC_compute_sha2_256(value x20, value x19, value x18,
                                    value x17, value x16)
{
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   unsigned char* x22 = CTYPES_PTR_OF_OCAML_BYTES(x19);
   uint32_t x23 = Uint32_val(x18);
   unsigned char* x26 = CTYPES_PTR_OF_OCAML_BYTES(x17);
   uint32_t x27 = Uint32_val(x16);
   Hacl_HMAC_compute_sha2_256(x21, x22, x23, x26, x27);
   return Val_unit;
}
value _3_Hacl_HMAC_compute_sha2_384(value x35, value x34, value x33,
                                    value x32, value x31)
{
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x34);
   uint32_t x38 = Uint32_val(x33);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x32);
   uint32_t x42 = Uint32_val(x31);
   Hacl_HMAC_compute_sha2_384(x36, x37, x38, x41, x42);
   return Val_unit;
}
value _4_Hacl_HMAC_compute_sha2_512(value x50, value x49, value x48,
                                    value x47, value x46)
{
   unsigned char* x51 = CTYPES_PTR_OF_OCAML_BYTES(x50);
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x49);
   uint32_t x53 = Uint32_val(x48);
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   uint32_t x57 = Uint32_val(x46);
   Hacl_HMAC_compute_sha2_512(x51, x52, x53, x56, x57);
   return Val_unit;
}
value _5_Hacl_HMAC_compute_blake2s_32(value x65, value x64, value x63,
                                      value x62, value x61)
{
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x65);
   unsigned char* x67 = CTYPES_PTR_OF_OCAML_BYTES(x64);
   uint32_t x68 = Uint32_val(x63);
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x62);
   uint32_t x72 = Uint32_val(x61);
   Hacl_HMAC_compute_blake2s_32(x66, x67, x68, x71, x72);
   return Val_unit;
}
value _6_Hacl_HMAC_compute_blake2b_32(value x80, value x79, value x78,
                                      value x77, value x76)
{
   unsigned char* x81 = CTYPES_PTR_OF_OCAML_BYTES(x80);
   unsigned char* x82 = CTYPES_PTR_OF_OCAML_BYTES(x79);
   uint32_t x83 = Uint32_val(x78);
   unsigned char* x86 = CTYPES_PTR_OF_OCAML_BYTES(x77);
   uint32_t x87 = Uint32_val(x76);
   Hacl_HMAC_compute_blake2b_32(x81, x82, x83, x86, x87);
   return Val_unit;
}
