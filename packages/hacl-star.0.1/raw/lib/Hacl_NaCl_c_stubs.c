
#include "Hacl_NaCl.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_NaCl_crypto_secretbox_detached(value x6, value x5, value x4,
                                             value x3, value x2, value x1)
{
   char* x7 = CTYPES_PTR_OF_OCAML_STRING(x6);
   char* x8 = CTYPES_PTR_OF_OCAML_STRING(x5);
   char* x9 = CTYPES_PTR_OF_OCAML_STRING(x4);
   uint32_t x10 = Uint32_val(x3);
   char* x13 = CTYPES_PTR_OF_OCAML_STRING(x2);
   char* x14 = CTYPES_PTR_OF_OCAML_STRING(x1);
   uint32_t x15 =
   Hacl_NaCl_crypto_secretbox_detached(x7, x8, x9, x10, x13, x14);
   return integers_copy_uint32(x15);
}
value _1_Hacl_NaCl_crypto_secretbox_detached_byte6(value* argv, int argc)
{
   value x16 = argv[5];
   value x17 = argv[4];
   value x18 = argv[3];
   value x19 = argv[2];
   value x20 = argv[1];
   value x21 = argv[0];
   return
     _1_Hacl_NaCl_crypto_secretbox_detached(x21, x20, x19, x18, x17, x16);
}
value _2_Hacl_NaCl_crypto_secretbox_open_detached(value x27, value x26,
                                                  value x25, value x24,
                                                  value x23, value x22)
{
   char* x28 = CTYPES_PTR_OF_OCAML_STRING(x27);
   char* x29 = CTYPES_PTR_OF_OCAML_STRING(x26);
   char* x30 = CTYPES_PTR_OF_OCAML_STRING(x25);
   uint32_t x31 = Uint32_val(x24);
   char* x34 = CTYPES_PTR_OF_OCAML_STRING(x23);
   char* x35 = CTYPES_PTR_OF_OCAML_STRING(x22);
   uint32_t x36 =
   Hacl_NaCl_crypto_secretbox_open_detached(x28, x29, x30, x31, x34, x35);
   return integers_copy_uint32(x36);
}
value _2_Hacl_NaCl_crypto_secretbox_open_detached_byte6(value* argv,
                                                        int argc)
{
   value x37 = argv[5];
   value x38 = argv[4];
   value x39 = argv[3];
   value x40 = argv[2];
   value x41 = argv[1];
   value x42 = argv[0];
   return
     _2_Hacl_NaCl_crypto_secretbox_open_detached(x42, x41, x40, x39, 
                                                 x38, x37);
}
value _3_Hacl_NaCl_crypto_secretbox_easy(value x47, value x46, value x45,
                                         value x44, value x43)
{
   char* x48 = CTYPES_PTR_OF_OCAML_STRING(x47);
   char* x49 = CTYPES_PTR_OF_OCAML_STRING(x46);
   uint32_t x50 = Uint32_val(x45);
   char* x53 = CTYPES_PTR_OF_OCAML_STRING(x44);
   char* x54 = CTYPES_PTR_OF_OCAML_STRING(x43);
   uint32_t x55 = Hacl_NaCl_crypto_secretbox_easy(x48, x49, x50, x53, x54);
   return integers_copy_uint32(x55);
}
value _4_Hacl_NaCl_crypto_secretbox_open_easy(value x60, value x59,
                                              value x58, value x57,
                                              value x56)
{
   char* x61 = CTYPES_PTR_OF_OCAML_STRING(x60);
   char* x62 = CTYPES_PTR_OF_OCAML_STRING(x59);
   uint32_t x63 = Uint32_val(x58);
   char* x66 = CTYPES_PTR_OF_OCAML_STRING(x57);
   char* x67 = CTYPES_PTR_OF_OCAML_STRING(x56);
   uint32_t x68 =
   Hacl_NaCl_crypto_secretbox_open_easy(x61, x62, x63, x66, x67);
   return integers_copy_uint32(x68);
}
value _5_Hacl_NaCl_crypto_box_beforenm(value x71, value x70, value x69)
{
   char* x72 = CTYPES_PTR_OF_OCAML_STRING(x71);
   char* x73 = CTYPES_PTR_OF_OCAML_STRING(x70);
   char* x74 = CTYPES_PTR_OF_OCAML_STRING(x69);
   uint32_t x75 = Hacl_NaCl_crypto_box_beforenm(x72, x73, x74);
   return integers_copy_uint32(x75);
}
value _6_Hacl_NaCl_crypto_box_detached_afternm(value x81, value x80,
                                               value x79, value x78,
                                               value x77, value x76)
{
   char* x82 = CTYPES_PTR_OF_OCAML_STRING(x81);
   char* x83 = CTYPES_PTR_OF_OCAML_STRING(x80);
   char* x84 = CTYPES_PTR_OF_OCAML_STRING(x79);
   uint32_t x85 = Uint32_val(x78);
   char* x88 = CTYPES_PTR_OF_OCAML_STRING(x77);
   char* x89 = CTYPES_PTR_OF_OCAML_STRING(x76);
   uint32_t x90 =
   Hacl_NaCl_crypto_box_detached_afternm(x82, x83, x84, x85, x88, x89);
   return integers_copy_uint32(x90);
}
value _6_Hacl_NaCl_crypto_box_detached_afternm_byte6(value* argv, int argc)
{
   value x91 = argv[5];
   value x92 = argv[4];
   value x93 = argv[3];
   value x94 = argv[2];
   value x95 = argv[1];
   value x96 = argv[0];
   return
     _6_Hacl_NaCl_crypto_box_detached_afternm(x96, x95, x94, x93, x92, x91);
}
value _7_Hacl_NaCl_crypto_box_detached(value x103, value x102, value x101,
                                       value x100, value x99, value x98,
                                       value x97)
{
   char* x104 = CTYPES_PTR_OF_OCAML_STRING(x103);
   char* x105 = CTYPES_PTR_OF_OCAML_STRING(x102);
   char* x106 = CTYPES_PTR_OF_OCAML_STRING(x101);
   uint32_t x107 = Uint32_val(x100);
   char* x110 = CTYPES_PTR_OF_OCAML_STRING(x99);
   char* x111 = CTYPES_PTR_OF_OCAML_STRING(x98);
   char* x112 = CTYPES_PTR_OF_OCAML_STRING(x97);
   uint32_t x113 =
   Hacl_NaCl_crypto_box_detached(x104, x105, x106, x107, x110, x111, x112);
   return integers_copy_uint32(x113);
}
value _7_Hacl_NaCl_crypto_box_detached_byte7(value* argv, int argc)
{
   value x114 = argv[6];
   value x115 = argv[5];
   value x116 = argv[4];
   value x117 = argv[3];
   value x118 = argv[2];
   value x119 = argv[1];
   value x120 = argv[0];
   return
     _7_Hacl_NaCl_crypto_box_detached(x120, x119, x118, x117, x116, x115,
                                      x114);
}
value _8_Hacl_NaCl_crypto_box_open_detached_afternm(value x126, value x125,
                                                    value x124, value x123,
                                                    value x122, value x121)
{
   char* x127 = CTYPES_PTR_OF_OCAML_STRING(x126);
   char* x128 = CTYPES_PTR_OF_OCAML_STRING(x125);
   char* x129 = CTYPES_PTR_OF_OCAML_STRING(x124);
   uint32_t x130 = Uint32_val(x123);
   char* x133 = CTYPES_PTR_OF_OCAML_STRING(x122);
   char* x134 = CTYPES_PTR_OF_OCAML_STRING(x121);
   uint32_t x135 =
   Hacl_NaCl_crypto_box_open_detached_afternm(x127, x128, x129, x130, 
                                              x133, x134);
   return integers_copy_uint32(x135);
}
value _8_Hacl_NaCl_crypto_box_open_detached_afternm_byte6(value* argv,
                                                          int argc)
{
   value x136 = argv[5];
   value x137 = argv[4];
   value x138 = argv[3];
   value x139 = argv[2];
   value x140 = argv[1];
   value x141 = argv[0];
   return
     _8_Hacl_NaCl_crypto_box_open_detached_afternm(x141, x140, x139, 
                                                   x138, x137, x136);
}
value _9_Hacl_NaCl_crypto_box_open_detached(value x148, value x147,
                                            value x146, value x145,
                                            value x144, value x143,
                                            value x142)
{
   char* x149 = CTYPES_PTR_OF_OCAML_STRING(x148);
   char* x150 = CTYPES_PTR_OF_OCAML_STRING(x147);
   char* x151 = CTYPES_PTR_OF_OCAML_STRING(x146);
   uint32_t x152 = Uint32_val(x145);
   char* x155 = CTYPES_PTR_OF_OCAML_STRING(x144);
   char* x156 = CTYPES_PTR_OF_OCAML_STRING(x143);
   char* x157 = CTYPES_PTR_OF_OCAML_STRING(x142);
   uint32_t x158 =
   Hacl_NaCl_crypto_box_open_detached(x149, x150, x151, x152, x155, x156,
                                      x157);
   return integers_copy_uint32(x158);
}
value _9_Hacl_NaCl_crypto_box_open_detached_byte7(value* argv, int argc)
{
   value x159 = argv[6];
   value x160 = argv[5];
   value x161 = argv[4];
   value x162 = argv[3];
   value x163 = argv[2];
   value x164 = argv[1];
   value x165 = argv[0];
   return
     _9_Hacl_NaCl_crypto_box_open_detached(x165, x164, x163, x162, x161,
                                           x160, x159);
}
value _10_Hacl_NaCl_crypto_box_easy_afternm(value x170, value x169,
                                            value x168, value x167,
                                            value x166)
{
   char* x171 = CTYPES_PTR_OF_OCAML_STRING(x170);
   char* x172 = CTYPES_PTR_OF_OCAML_STRING(x169);
   uint32_t x173 = Uint32_val(x168);
   char* x176 = CTYPES_PTR_OF_OCAML_STRING(x167);
   char* x177 = CTYPES_PTR_OF_OCAML_STRING(x166);
   uint32_t x178 =
   Hacl_NaCl_crypto_box_easy_afternm(x171, x172, x173, x176, x177);
   return integers_copy_uint32(x178);
}
value _11_Hacl_NaCl_crypto_box_easy(value x184, value x183, value x182,
                                    value x181, value x180, value x179)
{
   char* x185 = CTYPES_PTR_OF_OCAML_STRING(x184);
   char* x186 = CTYPES_PTR_OF_OCAML_STRING(x183);
   uint32_t x187 = Uint32_val(x182);
   char* x190 = CTYPES_PTR_OF_OCAML_STRING(x181);
   char* x191 = CTYPES_PTR_OF_OCAML_STRING(x180);
   char* x192 = CTYPES_PTR_OF_OCAML_STRING(x179);
   uint32_t x193 =
   Hacl_NaCl_crypto_box_easy(x185, x186, x187, x190, x191, x192);
   return integers_copy_uint32(x193);
}
value _11_Hacl_NaCl_crypto_box_easy_byte6(value* argv, int argc)
{
   value x194 = argv[5];
   value x195 = argv[4];
   value x196 = argv[3];
   value x197 = argv[2];
   value x198 = argv[1];
   value x199 = argv[0];
   return _11_Hacl_NaCl_crypto_box_easy(x199, x198, x197, x196, x195, x194);
}
value _12_Hacl_NaCl_crypto_box_open_easy_afternm(value x204, value x203,
                                                 value x202, value x201,
                                                 value x200)
{
   char* x205 = CTYPES_PTR_OF_OCAML_STRING(x204);
   char* x206 = CTYPES_PTR_OF_OCAML_STRING(x203);
   uint32_t x207 = Uint32_val(x202);
   char* x210 = CTYPES_PTR_OF_OCAML_STRING(x201);
   char* x211 = CTYPES_PTR_OF_OCAML_STRING(x200);
   uint32_t x212 =
   Hacl_NaCl_crypto_box_open_easy_afternm(x205, x206, x207, x210, x211);
   return integers_copy_uint32(x212);
}
value _13_Hacl_NaCl_crypto_box_open_easy(value x218, value x217, value x216,
                                         value x215, value x214, value x213)
{
   char* x219 = CTYPES_PTR_OF_OCAML_STRING(x218);
   char* x220 = CTYPES_PTR_OF_OCAML_STRING(x217);
   uint32_t x221 = Uint32_val(x216);
   char* x224 = CTYPES_PTR_OF_OCAML_STRING(x215);
   char* x225 = CTYPES_PTR_OF_OCAML_STRING(x214);
   char* x226 = CTYPES_PTR_OF_OCAML_STRING(x213);
   uint32_t x227 =
   Hacl_NaCl_crypto_box_open_easy(x219, x220, x221, x224, x225, x226);
   return integers_copy_uint32(x227);
}
value _13_Hacl_NaCl_crypto_box_open_easy_byte6(value* argv, int argc)
{
   value x228 = argv[5];
   value x229 = argv[4];
   value x230 = argv[3];
   value x231 = argv[2];
   value x232 = argv[1];
   value x233 = argv[0];
   return
     _13_Hacl_NaCl_crypto_box_open_easy(x233, x232, x231, x230, x229, x228);
}
