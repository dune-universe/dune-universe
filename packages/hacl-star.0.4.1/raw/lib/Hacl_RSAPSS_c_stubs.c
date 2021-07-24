
#include "Hacl_RSAPSS.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_RSAPSS_rsapss_sign(value x10, value x9, value x8, value x7,
                                 value x6, value x5, value x4, value x3,
                                 value x2, value x1)
{
   uint8_t x11 = Uint8_val(x10);
   uint32_t x14 = Uint32_val(x9);
   uint32_t x17 = Uint32_val(x8);
   uint32_t x20 = Uint32_val(x7);
   uint64_t* x23 = CTYPES_ADDR_OF_FATPTR(x6);
   uint32_t x24 = Uint32_val(x5);
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x28 = Uint32_val(x3);
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   _Bool x33 =
   Hacl_RSAPSS_rsapss_sign(x11, x14, x17, x20, x23, x24, x27, x28, x31, x32);
   return Val_bool(x33);
}
value _1_Hacl_RSAPSS_rsapss_sign_byte10(value* argv, int argc)
{
   value x34 = argv[9];
   value x35 = argv[8];
   value x36 = argv[7];
   value x37 = argv[6];
   value x38 = argv[5];
   value x39 = argv[4];
   value x40 = argv[3];
   value x41 = argv[2];
   value x42 = argv[1];
   value x43 = argv[0];
   return
     _1_Hacl_RSAPSS_rsapss_sign(x43, x42, x41, x40, x39, x38, x37, x36, 
                                x35, x34);
}
value _2_Hacl_RSAPSS_rsapss_verify(value x52, value x51, value x50,
                                   value x49, value x48, value x47,
                                   value x46, value x45, value x44)
{
   uint8_t x53 = Uint8_val(x52);
   uint32_t x56 = Uint32_val(x51);
   uint32_t x59 = Uint32_val(x50);
   uint64_t* x62 = CTYPES_ADDR_OF_FATPTR(x49);
   uint32_t x63 = Uint32_val(x48);
   uint32_t x66 = Uint32_val(x47);
   unsigned char* x69 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   uint32_t x70 = Uint32_val(x45);
   unsigned char* x73 = CTYPES_PTR_OF_OCAML_BYTES(x44);
   _Bool x74 =
   Hacl_RSAPSS_rsapss_verify(x53, x56, x59, x62, x63, x66, x69, x70, x73);
   return Val_bool(x74);
}
value _2_Hacl_RSAPSS_rsapss_verify_byte9(value* argv, int argc)
{
   value x75 = argv[8];
   value x76 = argv[7];
   value x77 = argv[6];
   value x78 = argv[5];
   value x79 = argv[4];
   value x80 = argv[3];
   value x81 = argv[2];
   value x82 = argv[1];
   value x83 = argv[0];
   return
     _2_Hacl_RSAPSS_rsapss_verify(x83, x82, x81, x80, x79, x78, x77, 
                                  x76, x75);
}
value _3_Hacl_RSAPSS_new_rsapss_load_pkey(value x87, value x86, value x85,
                                          value x84)
{
   uint32_t x88 = Uint32_val(x87);
   uint32_t x91 = Uint32_val(x86);
   unsigned char* x94 = CTYPES_PTR_OF_OCAML_BYTES(x85);
   unsigned char* x95 = CTYPES_PTR_OF_OCAML_BYTES(x84);
   uint64_t* x96 = Hacl_RSAPSS_new_rsapss_load_pkey(x88, x91, x94, x95);
   return CTYPES_FROM_PTR(x96);
}
value _4_Hacl_RSAPSS_new_rsapss_load_skey(value x102, value x101, value x100,
                                          value x99, value x98, value x97)
{
   uint32_t x103 = Uint32_val(x102);
   uint32_t x106 = Uint32_val(x101);
   uint32_t x109 = Uint32_val(x100);
   unsigned char* x112 = CTYPES_PTR_OF_OCAML_BYTES(x99);
   unsigned char* x113 = CTYPES_PTR_OF_OCAML_BYTES(x98);
   unsigned char* x114 = CTYPES_PTR_OF_OCAML_BYTES(x97);
   uint64_t* x115 =
   Hacl_RSAPSS_new_rsapss_load_skey(x103, x106, x109, x112, x113, x114);
   return CTYPES_FROM_PTR(x115);
}
value _4_Hacl_RSAPSS_new_rsapss_load_skey_byte6(value* argv, int argc)
{
   value x116 = argv[5];
   value x117 = argv[4];
   value x118 = argv[3];
   value x119 = argv[2];
   value x120 = argv[1];
   value x121 = argv[0];
   return
     _4_Hacl_RSAPSS_new_rsapss_load_skey(x121, x120, x119, x118, x117, x116);
}
value _5_Hacl_RSAPSS_rsapss_skey_sign(value x133, value x132, value x131,
                                      value x130, value x129, value x128,
                                      value x127, value x126, value x125,
                                      value x124, value x123, value x122)
{
   uint8_t x134 = Uint8_val(x133);
   uint32_t x137 = Uint32_val(x132);
   uint32_t x140 = Uint32_val(x131);
   uint32_t x143 = Uint32_val(x130);
   unsigned char* x146 = CTYPES_PTR_OF_OCAML_BYTES(x129);
   unsigned char* x147 = CTYPES_PTR_OF_OCAML_BYTES(x128);
   unsigned char* x148 = CTYPES_PTR_OF_OCAML_BYTES(x127);
   uint32_t x149 = Uint32_val(x126);
   unsigned char* x152 = CTYPES_PTR_OF_OCAML_BYTES(x125);
   uint32_t x153 = Uint32_val(x124);
   unsigned char* x156 = CTYPES_PTR_OF_OCAML_BYTES(x123);
   unsigned char* x157 = CTYPES_PTR_OF_OCAML_BYTES(x122);
   _Bool x158 =
   Hacl_RSAPSS_rsapss_skey_sign(x134, x137, x140, x143, x146, x147, x148,
                                x149, x152, x153, x156, x157);
   return Val_bool(x158);
}
value _5_Hacl_RSAPSS_rsapss_skey_sign_byte12(value* argv, int argc)
{
   value x159 = argv[11];
   value x160 = argv[10];
   value x161 = argv[9];
   value x162 = argv[8];
   value x163 = argv[7];
   value x164 = argv[6];
   value x165 = argv[5];
   value x166 = argv[4];
   value x167 = argv[3];
   value x168 = argv[2];
   value x169 = argv[1];
   value x170 = argv[0];
   return
     _5_Hacl_RSAPSS_rsapss_skey_sign(x170, x169, x168, x167, x166, x165,
                                     x164, x163, x162, x161, x160, x159);
}
value _6_Hacl_RSAPSS_rsapss_pkey_verify(value x180, value x179, value x178,
                                        value x177, value x176, value x175,
                                        value x174, value x173, value x172,
                                        value x171)
{
   uint8_t x181 = Uint8_val(x180);
   uint32_t x184 = Uint32_val(x179);
   uint32_t x187 = Uint32_val(x178);
   unsigned char* x190 = CTYPES_PTR_OF_OCAML_BYTES(x177);
   unsigned char* x191 = CTYPES_PTR_OF_OCAML_BYTES(x176);
   uint32_t x192 = Uint32_val(x175);
   uint32_t x195 = Uint32_val(x174);
   unsigned char* x198 = CTYPES_PTR_OF_OCAML_BYTES(x173);
   uint32_t x199 = Uint32_val(x172);
   unsigned char* x202 = CTYPES_PTR_OF_OCAML_BYTES(x171);
   _Bool x203 =
   Hacl_RSAPSS_rsapss_pkey_verify(x181, x184, x187, x190, x191, x192, 
                                  x195, x198, x199, x202);
   return Val_bool(x203);
}
value _6_Hacl_RSAPSS_rsapss_pkey_verify_byte10(value* argv, int argc)
{
   value x204 = argv[9];
   value x205 = argv[8];
   value x206 = argv[7];
   value x207 = argv[6];
   value x208 = argv[5];
   value x209 = argv[4];
   value x210 = argv[3];
   value x211 = argv[2];
   value x212 = argv[1];
   value x213 = argv[0];
   return
     _6_Hacl_RSAPSS_rsapss_pkey_verify(x213, x212, x211, x210, x209, 
                                       x208, x207, x206, x205, x204);
}
