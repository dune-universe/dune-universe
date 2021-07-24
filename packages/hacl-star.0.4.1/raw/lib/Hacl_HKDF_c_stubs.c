
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
value _5_Hacl_HKDF_expand_blake2s_32(value x86, value x85, value x84,
                                     value x83, value x82, value x81)
{
   unsigned char* x87 = CTYPES_PTR_OF_OCAML_BYTES(x86);
   unsigned char* x88 = CTYPES_PTR_OF_OCAML_BYTES(x85);
   uint32_t x89 = Uint32_val(x84);
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x83);
   uint32_t x93 = Uint32_val(x82);
   uint32_t x96 = Uint32_val(x81);
   Hacl_HKDF_expand_blake2s_32(x87, x88, x89, x92, x93, x96);
   return Val_unit;
}
value _5_Hacl_HKDF_expand_blake2s_32_byte6(value* argv, int argc)
{
   value x100 = argv[5];
   value x101 = argv[4];
   value x102 = argv[3];
   value x103 = argv[2];
   value x104 = argv[1];
   value x105 = argv[0];
   return _5_Hacl_HKDF_expand_blake2s_32(x105, x104, x103, x102, x101, x100);
}
value _6_Hacl_HKDF_extract_blake2s_32(value x110, value x109, value x108,
                                      value x107, value x106)
{
   unsigned char* x111 = CTYPES_PTR_OF_OCAML_BYTES(x110);
   unsigned char* x112 = CTYPES_PTR_OF_OCAML_BYTES(x109);
   uint32_t x113 = Uint32_val(x108);
   unsigned char* x116 = CTYPES_PTR_OF_OCAML_BYTES(x107);
   uint32_t x117 = Uint32_val(x106);
   Hacl_HKDF_extract_blake2s_32(x111, x112, x113, x116, x117);
   return Val_unit;
}
value _7_Hacl_HKDF_expand_blake2b_32(value x126, value x125, value x124,
                                     value x123, value x122, value x121)
{
   unsigned char* x127 = CTYPES_PTR_OF_OCAML_BYTES(x126);
   unsigned char* x128 = CTYPES_PTR_OF_OCAML_BYTES(x125);
   uint32_t x129 = Uint32_val(x124);
   unsigned char* x132 = CTYPES_PTR_OF_OCAML_BYTES(x123);
   uint32_t x133 = Uint32_val(x122);
   uint32_t x136 = Uint32_val(x121);
   Hacl_HKDF_expand_blake2b_32(x127, x128, x129, x132, x133, x136);
   return Val_unit;
}
value _7_Hacl_HKDF_expand_blake2b_32_byte6(value* argv, int argc)
{
   value x140 = argv[5];
   value x141 = argv[4];
   value x142 = argv[3];
   value x143 = argv[2];
   value x144 = argv[1];
   value x145 = argv[0];
   return _7_Hacl_HKDF_expand_blake2b_32(x145, x144, x143, x142, x141, x140);
}
value _8_Hacl_HKDF_extract_blake2b_32(value x150, value x149, value x148,
                                      value x147, value x146)
{
   unsigned char* x151 = CTYPES_PTR_OF_OCAML_BYTES(x150);
   unsigned char* x152 = CTYPES_PTR_OF_OCAML_BYTES(x149);
   uint32_t x153 = Uint32_val(x148);
   unsigned char* x156 = CTYPES_PTR_OF_OCAML_BYTES(x147);
   uint32_t x157 = Uint32_val(x146);
   Hacl_HKDF_extract_blake2b_32(x151, x152, x153, x156, x157);
   return Val_unit;
}
