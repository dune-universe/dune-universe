
#include "Hacl_RSAPSS2048_SHA256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_RSAPSS2048_SHA256_rsapss_sign(value x8, value x7, value x6,
                                            value x5, value x4, value x3,
                                            value x2, value x1)
{
   uint32_t x9 = Uint32_val(x8);
   uint32_t x12 = Uint32_val(x7);
   uint64_t* x15 = CTYPES_ADDR_OF_FATPTR(x6);
   uint32_t x16 = Uint32_val(x5);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x20 = Uint32_val(x3);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   unsigned char* x24 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   _Bool x25 =
   Hacl_RSAPSS2048_SHA256_rsapss_sign(x9, x12, x15, x16, x19, x20, x23, x24);
   return Val_bool(x25);
}
value _1_Hacl_RSAPSS2048_SHA256_rsapss_sign_byte8(value* argv, int argc)
{
   value x26 = argv[7];
   value x27 = argv[6];
   value x28 = argv[5];
   value x29 = argv[4];
   value x30 = argv[3];
   value x31 = argv[2];
   value x32 = argv[1];
   value x33 = argv[0];
   return
     _1_Hacl_RSAPSS2048_SHA256_rsapss_sign(x33, x32, x31, x30, x29, x28, 
                                           x27, x26);
}
value _2_Hacl_RSAPSS2048_SHA256_rsapss_verify(value x40, value x39,
                                              value x38, value x37,
                                              value x36, value x35,
                                              value x34)
{
   uint32_t x41 = Uint32_val(x40);
   uint64_t* x44 = CTYPES_ADDR_OF_FATPTR(x39);
   uint32_t x45 = Uint32_val(x38);
   uint32_t x48 = Uint32_val(x37);
   unsigned char* x51 = CTYPES_PTR_OF_OCAML_BYTES(x36);
   uint32_t x52 = Uint32_val(x35);
   unsigned char* x55 = CTYPES_PTR_OF_OCAML_BYTES(x34);
   _Bool x56 =
   Hacl_RSAPSS2048_SHA256_rsapss_verify(x41, x44, x45, x48, x51, x52, x55);
   return Val_bool(x56);
}
value _2_Hacl_RSAPSS2048_SHA256_rsapss_verify_byte7(value* argv, int argc)
{
   value x57 = argv[6];
   value x58 = argv[5];
   value x59 = argv[4];
   value x60 = argv[3];
   value x61 = argv[2];
   value x62 = argv[1];
   value x63 = argv[0];
   return
     _2_Hacl_RSAPSS2048_SHA256_rsapss_verify(x63, x62, x61, x60, x59, 
                                             x58, x57);
}
value _3_Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey(value x66, value x65,
                                                     value x64)
{
   uint32_t x67 = Uint32_val(x66);
   unsigned char* x70 = CTYPES_PTR_OF_OCAML_BYTES(x65);
   unsigned char* x71 = CTYPES_PTR_OF_OCAML_BYTES(x64);
   uint64_t* x72 =
   Hacl_RSAPSS2048_SHA256_new_rsapss_load_pkey(x67, x70, x71);
   return CTYPES_FROM_PTR(x72);
}
value _4_Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey(value x77, value x76,
                                                     value x75, value x74,
                                                     value x73)
{
   uint32_t x78 = Uint32_val(x77);
   uint32_t x81 = Uint32_val(x76);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x75);
   unsigned char* x85 = CTYPES_PTR_OF_OCAML_BYTES(x74);
   unsigned char* x86 = CTYPES_PTR_OF_OCAML_BYTES(x73);
   uint64_t* x87 =
   Hacl_RSAPSS2048_SHA256_new_rsapss_load_skey(x78, x81, x84, x85, x86);
   return CTYPES_FROM_PTR(x87);
}
value _5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign(value x97, value x96,
                                                 value x95, value x94,
                                                 value x93, value x92,
                                                 value x91, value x90,
                                                 value x89, value x88)
{
   uint32_t x98 = Uint32_val(x97);
   uint32_t x101 = Uint32_val(x96);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x95);
   unsigned char* x105 = CTYPES_PTR_OF_OCAML_BYTES(x94);
   unsigned char* x106 = CTYPES_PTR_OF_OCAML_BYTES(x93);
   uint32_t x107 = Uint32_val(x92);
   unsigned char* x110 = CTYPES_PTR_OF_OCAML_BYTES(x91);
   uint32_t x111 = Uint32_val(x90);
   unsigned char* x114 = CTYPES_PTR_OF_OCAML_BYTES(x89);
   unsigned char* x115 = CTYPES_PTR_OF_OCAML_BYTES(x88);
   _Bool x116 =
   Hacl_RSAPSS2048_SHA256_rsapss_skey_sign(x98, x101, x104, x105, x106, 
                                           x107, x110, x111, x114, x115);
   return Val_bool(x116);
}
value _5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign_byte10(value* argv,
                                                        int argc)
{
   value x117 = argv[9];
   value x118 = argv[8];
   value x119 = argv[7];
   value x120 = argv[6];
   value x121 = argv[5];
   value x122 = argv[4];
   value x123 = argv[3];
   value x124 = argv[2];
   value x125 = argv[1];
   value x126 = argv[0];
   return
     _5_Hacl_RSAPSS2048_SHA256_rsapss_skey_sign(x126, x125, x124, x123, 
                                                x122, x121, x120, x119, 
                                                x118, x117);
}
value _6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify(value x134, value x133,
                                                   value x132, value x131,
                                                   value x130, value x129,
                                                   value x128, value x127)
{
   uint32_t x135 = Uint32_val(x134);
   unsigned char* x138 = CTYPES_PTR_OF_OCAML_BYTES(x133);
   unsigned char* x139 = CTYPES_PTR_OF_OCAML_BYTES(x132);
   uint32_t x140 = Uint32_val(x131);
   uint32_t x143 = Uint32_val(x130);
   unsigned char* x146 = CTYPES_PTR_OF_OCAML_BYTES(x129);
   uint32_t x147 = Uint32_val(x128);
   unsigned char* x150 = CTYPES_PTR_OF_OCAML_BYTES(x127);
   _Bool x151 =
   Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify(x135, x138, x139, x140, 
                                             x143, x146, x147, x150);
   return Val_bool(x151);
}
value _6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify_byte8(value* argv,
                                                         int argc)
{
   value x152 = argv[7];
   value x153 = argv[6];
   value x154 = argv[5];
   value x155 = argv[4];
   value x156 = argv[3];
   value x157 = argv[2];
   value x158 = argv[1];
   value x159 = argv[0];
   return
     _6_Hacl_RSAPSS2048_SHA256_rsapss_pkey_verify(x159, x158, x157, x156,
                                                  x155, x154, x153, x152);
}
