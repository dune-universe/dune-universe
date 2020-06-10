
#include "Hacl_HKDF.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HKDF_expand_sha2_256(value x6, value x5, value x4, value x3,
                                   value x2, value x1)
{
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   uint32_t x9 = Uint32_val(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x13 = Uint32_val(x2);
   uint32_t x16 = Uint32_val(x1);
   Hacl_HKDF_expand_sha2_256(x7, x8, x9, x12, x13, x16);
   return Val_unit;
}
value _1_Hacl_HKDF_expand_sha2_256_byte6(value* argv, int argc)
{
   value x20 = argv[5];
   value x21 = argv[4];
   value x22 = argv[3];
   value x23 = argv[2];
   value x24 = argv[1];
   value x25 = argv[0];
   return _1_Hacl_HKDF_expand_sha2_256(x25, x24, x23, x22, x21, x20);
}
value _2_Hacl_HKDF_extract_sha2_256(value x30, value x29, value x28,
                                    value x27, value x26)
{
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x30);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   uint32_t x33 = Uint32_val(x28);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint32_t x37 = Uint32_val(x26);
   Hacl_HKDF_extract_sha2_256(x31, x32, x33, x36, x37);
   return Val_unit;
}
value _3_Hacl_HKDF_expand_sha2_512(value x46, value x45, value x44,
                                   value x43, value x42, value x41)
{
   unsigned char* x47 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   unsigned char* x48 = CTYPES_PTR_OF_OCAML_BYTES(x45);
   uint32_t x49 = Uint32_val(x44);
   unsigned char* x52 = CTYPES_PTR_OF_OCAML_BYTES(x43);
   uint32_t x53 = Uint32_val(x42);
   uint32_t x56 = Uint32_val(x41);
   Hacl_HKDF_expand_sha2_512(x47, x48, x49, x52, x53, x56);
   return Val_unit;
}
value _3_Hacl_HKDF_expand_sha2_512_byte6(value* argv, int argc)
{
   value x60 = argv[5];
   value x61 = argv[4];
   value x62 = argv[3];
   value x63 = argv[2];
   value x64 = argv[1];
   value x65 = argv[0];
   return _3_Hacl_HKDF_expand_sha2_512(x65, x64, x63, x62, x61, x60);
}
value _4_Hacl_HKDF_extract_sha2_512(value x70, value x69, value x68,
                                    value x67, value x66)
{
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x70);
   unsigned char* x72 = CTYPES_PTR_OF_OCAML_BYTES(x69);
   uint32_t x73 = Uint32_val(x68);
   unsigned char* x76 = CTYPES_PTR_OF_OCAML_BYTES(x67);
   uint32_t x77 = Uint32_val(x66);
   Hacl_HKDF_extract_sha2_512(x71, x72, x73, x76, x77);
   return Val_unit;
}
