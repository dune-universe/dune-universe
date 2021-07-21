
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
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   EverCrypt_Error_error_code x12 = EverCrypt_AEAD_create_in(x7, x10, x11);
   return Integers_val_uint8(x12);
}
value _3_EverCrypt_AEAD_encrypt(value x21, value x20, value x19, value x18,
                                value x17, value x16, value x15, value x14,
                                value x13)
{
   struct EverCrypt_AEAD_state_s_s* x22 = CTYPES_ADDR_OF_FATPTR(x21);
   unsigned char* x23 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   uint32_t x24 = Uint32_val(x19);
   unsigned char* x27 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   uint32_t x28 = Uint32_val(x17);
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x16);
   uint32_t x32 = Uint32_val(x15);
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x13);
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
value _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm(value x55, value x54,
                                                  value x53, value x52,
                                                  value x51, value x50,
                                                  value x49, value x48,
                                                  value x47)
{
   unsigned char* x56 = CTYPES_PTR_OF_OCAML_BYTES(x55);
   unsigned char* x57 = CTYPES_PTR_OF_OCAML_BYTES(x54);
   uint32_t x58 = Uint32_val(x53);
   unsigned char* x61 = CTYPES_PTR_OF_OCAML_BYTES(x52);
   uint32_t x62 = Uint32_val(x51);
   unsigned char* x65 = CTYPES_PTR_OF_OCAML_BYTES(x50);
   uint32_t x66 = Uint32_val(x49);
   unsigned char* x69 = CTYPES_PTR_OF_OCAML_BYTES(x48);
   unsigned char* x70 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   EverCrypt_Error_error_code x71 =
   EverCrypt_AEAD_encrypt_expand_aes128_gcm(x56, x57, x58, x61, x62, 
                                            x65, x66, x69, x70);
   return Integers_val_uint8(x71);
}
value _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm_byte9(value* argv,
                                                        int argc)
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
     _4_EverCrypt_AEAD_encrypt_expand_aes128_gcm(x80, x79, x78, x77, 
                                                 x76, x75, x74, x73, 
                                                 x72);
}
value _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm(value x89, value x88,
                                                  value x87, value x86,
                                                  value x85, value x84,
                                                  value x83, value x82,
                                                  value x81)
{
   unsigned char* x90 = CTYPES_PTR_OF_OCAML_BYTES(x89);
   unsigned char* x91 = CTYPES_PTR_OF_OCAML_BYTES(x88);
   uint32_t x92 = Uint32_val(x87);
   unsigned char* x95 = CTYPES_PTR_OF_OCAML_BYTES(x86);
   uint32_t x96 = Uint32_val(x85);
   unsigned char* x99 = CTYPES_PTR_OF_OCAML_BYTES(x84);
   uint32_t x100 = Uint32_val(x83);
   unsigned char* x103 = CTYPES_PTR_OF_OCAML_BYTES(x82);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x81);
   EverCrypt_Error_error_code x105 =
   EverCrypt_AEAD_encrypt_expand_aes256_gcm(x90, x91, x92, x95, x96, 
                                            x99, x100, x103, x104);
   return Integers_val_uint8(x105);
}
value _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm_byte9(value* argv,
                                                        int argc)
{
   value x106 = argv[8];
   value x107 = argv[7];
   value x108 = argv[6];
   value x109 = argv[5];
   value x110 = argv[4];
   value x111 = argv[3];
   value x112 = argv[2];
   value x113 = argv[1];
   value x114 = argv[0];
   return
     _5_EverCrypt_AEAD_encrypt_expand_aes256_gcm(x114, x113, x112, x111,
                                                 x110, x109, x108, x107,
                                                 x106);
}
value _6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305(value x123,
                                                         value x122,
                                                         value x121,
                                                         value x120,
                                                         value x119,
                                                         value x118,
                                                         value x117,
                                                         value x116,
                                                         value x115)
{
   unsigned char* x124 = CTYPES_PTR_OF_OCAML_BYTES(x123);
   unsigned char* x125 = CTYPES_PTR_OF_OCAML_BYTES(x122);
   uint32_t x126 = Uint32_val(x121);
   unsigned char* x129 = CTYPES_PTR_OF_OCAML_BYTES(x120);
   uint32_t x130 = Uint32_val(x119);
   unsigned char* x133 = CTYPES_PTR_OF_OCAML_BYTES(x118);
   uint32_t x134 = Uint32_val(x117);
   unsigned char* x137 = CTYPES_PTR_OF_OCAML_BYTES(x116);
   unsigned char* x138 = CTYPES_PTR_OF_OCAML_BYTES(x115);
   EverCrypt_Error_error_code x139 =
   EverCrypt_AEAD_encrypt_expand_chacha20_poly1305(x124, x125, x126, 
                                                   x129, x130, x133, 
                                                   x134, x137, x138);
   return Integers_val_uint8(x139);
}
value _6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305_byte9(value* argv,
                                                               int argc)
{
   value x140 = argv[8];
   value x141 = argv[7];
   value x142 = argv[6];
   value x143 = argv[5];
   value x144 = argv[4];
   value x145 = argv[3];
   value x146 = argv[2];
   value x147 = argv[1];
   value x148 = argv[0];
   return
     _6_EverCrypt_AEAD_encrypt_expand_chacha20_poly1305(x148, x147, x146,
                                                        x145, x144, x143,
                                                        x142, x141, x140);
}
value _7_EverCrypt_AEAD_encrypt_expand(value x158, value x157, value x156,
                                       value x155, value x154, value x153,
                                       value x152, value x151, value x150,
                                       value x149)
{
   uint8_t x159 = Uint8_val(x158);
   unsigned char* x162 = CTYPES_PTR_OF_OCAML_BYTES(x157);
   unsigned char* x163 = CTYPES_PTR_OF_OCAML_BYTES(x156);
   uint32_t x164 = Uint32_val(x155);
   unsigned char* x167 = CTYPES_PTR_OF_OCAML_BYTES(x154);
   uint32_t x168 = Uint32_val(x153);
   unsigned char* x171 = CTYPES_PTR_OF_OCAML_BYTES(x152);
   uint32_t x172 = Uint32_val(x151);
   unsigned char* x175 = CTYPES_PTR_OF_OCAML_BYTES(x150);
   unsigned char* x176 = CTYPES_PTR_OF_OCAML_BYTES(x149);
   EverCrypt_Error_error_code x177 =
   EverCrypt_AEAD_encrypt_expand(x159, x162, x163, x164, x167, x168, 
                                 x171, x172, x175, x176);
   return Integers_val_uint8(x177);
}
value _7_EverCrypt_AEAD_encrypt_expand_byte10(value* argv, int argc)
{
   value x178 = argv[9];
   value x179 = argv[8];
   value x180 = argv[7];
   value x181 = argv[6];
   value x182 = argv[5];
   value x183 = argv[4];
   value x184 = argv[3];
   value x185 = argv[2];
   value x186 = argv[1];
   value x187 = argv[0];
   return
     _7_EverCrypt_AEAD_encrypt_expand(x187, x186, x185, x184, x183, x182,
                                      x181, x180, x179, x178);
}
value _8_EverCrypt_AEAD_decrypt(value x196, value x195, value x194,
                                value x193, value x192, value x191,
                                value x190, value x189, value x188)
{
   struct EverCrypt_AEAD_state_s_s* x197 = CTYPES_ADDR_OF_FATPTR(x196);
   unsigned char* x198 = CTYPES_PTR_OF_OCAML_BYTES(x195);
   uint32_t x199 = Uint32_val(x194);
   unsigned char* x202 = CTYPES_PTR_OF_OCAML_BYTES(x193);
   uint32_t x203 = Uint32_val(x192);
   unsigned char* x206 = CTYPES_PTR_OF_OCAML_BYTES(x191);
   uint32_t x207 = Uint32_val(x190);
   unsigned char* x210 = CTYPES_PTR_OF_OCAML_BYTES(x189);
   unsigned char* x211 = CTYPES_PTR_OF_OCAML_BYTES(x188);
   EverCrypt_Error_error_code x212 =
   EverCrypt_AEAD_decrypt(x197, x198, x199, x202, x203, x206, x207, x210,
                          x211);
   return Integers_val_uint8(x212);
}
value _8_EverCrypt_AEAD_decrypt_byte9(value* argv, int argc)
{
   value x213 = argv[8];
   value x214 = argv[7];
   value x215 = argv[6];
   value x216 = argv[5];
   value x217 = argv[4];
   value x218 = argv[3];
   value x219 = argv[2];
   value x220 = argv[1];
   value x221 = argv[0];
   return
     _8_EverCrypt_AEAD_decrypt(x221, x220, x219, x218, x217, x216, x215,
                               x214, x213);
}
value _9_EverCrypt_AEAD_decrypt_expand_aes128_gcm(value x230, value x229,
                                                  value x228, value x227,
                                                  value x226, value x225,
                                                  value x224, value x223,
                                                  value x222)
{
   unsigned char* x231 = CTYPES_PTR_OF_OCAML_BYTES(x230);
   unsigned char* x232 = CTYPES_PTR_OF_OCAML_BYTES(x229);
   uint32_t x233 = Uint32_val(x228);
   unsigned char* x236 = CTYPES_PTR_OF_OCAML_BYTES(x227);
   uint32_t x237 = Uint32_val(x226);
   unsigned char* x240 = CTYPES_PTR_OF_OCAML_BYTES(x225);
   uint32_t x241 = Uint32_val(x224);
   unsigned char* x244 = CTYPES_PTR_OF_OCAML_BYTES(x223);
   unsigned char* x245 = CTYPES_PTR_OF_OCAML_BYTES(x222);
   EverCrypt_Error_error_code x246 =
   EverCrypt_AEAD_decrypt_expand_aes128_gcm(x231, x232, x233, x236, x237,
                                            x240, x241, x244, x245);
   return Integers_val_uint8(x246);
}
value _9_EverCrypt_AEAD_decrypt_expand_aes128_gcm_byte9(value* argv,
                                                        int argc)
{
   value x247 = argv[8];
   value x248 = argv[7];
   value x249 = argv[6];
   value x250 = argv[5];
   value x251 = argv[4];
   value x252 = argv[3];
   value x253 = argv[2];
   value x254 = argv[1];
   value x255 = argv[0];
   return
     _9_EverCrypt_AEAD_decrypt_expand_aes128_gcm(x255, x254, x253, x252,
                                                 x251, x250, x249, x248,
                                                 x247);
}
value _10_EverCrypt_AEAD_decrypt_expand_aes256_gcm(value x264, value x263,
                                                   value x262, value x261,
                                                   value x260, value x259,
                                                   value x258, value x257,
                                                   value x256)
{
   unsigned char* x265 = CTYPES_PTR_OF_OCAML_BYTES(x264);
   unsigned char* x266 = CTYPES_PTR_OF_OCAML_BYTES(x263);
   uint32_t x267 = Uint32_val(x262);
   unsigned char* x270 = CTYPES_PTR_OF_OCAML_BYTES(x261);
   uint32_t x271 = Uint32_val(x260);
   unsigned char* x274 = CTYPES_PTR_OF_OCAML_BYTES(x259);
   uint32_t x275 = Uint32_val(x258);
   unsigned char* x278 = CTYPES_PTR_OF_OCAML_BYTES(x257);
   unsigned char* x279 = CTYPES_PTR_OF_OCAML_BYTES(x256);
   EverCrypt_Error_error_code x280 =
   EverCrypt_AEAD_decrypt_expand_aes256_gcm(x265, x266, x267, x270, x271,
                                            x274, x275, x278, x279);
   return Integers_val_uint8(x280);
}
value _10_EverCrypt_AEAD_decrypt_expand_aes256_gcm_byte9(value* argv,
                                                         int argc)
{
   value x281 = argv[8];
   value x282 = argv[7];
   value x283 = argv[6];
   value x284 = argv[5];
   value x285 = argv[4];
   value x286 = argv[3];
   value x287 = argv[2];
   value x288 = argv[1];
   value x289 = argv[0];
   return
     _10_EverCrypt_AEAD_decrypt_expand_aes256_gcm(x289, x288, x287, x286,
                                                  x285, x284, x283, x282,
                                                  x281);
}
value _11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305(value x298,
                                                          value x297,
                                                          value x296,
                                                          value x295,
                                                          value x294,
                                                          value x293,
                                                          value x292,
                                                          value x291,
                                                          value x290)
{
   unsigned char* x299 = CTYPES_PTR_OF_OCAML_BYTES(x298);
   unsigned char* x300 = CTYPES_PTR_OF_OCAML_BYTES(x297);
   uint32_t x301 = Uint32_val(x296);
   unsigned char* x304 = CTYPES_PTR_OF_OCAML_BYTES(x295);
   uint32_t x305 = Uint32_val(x294);
   unsigned char* x308 = CTYPES_PTR_OF_OCAML_BYTES(x293);
   uint32_t x309 = Uint32_val(x292);
   unsigned char* x312 = CTYPES_PTR_OF_OCAML_BYTES(x291);
   unsigned char* x313 = CTYPES_PTR_OF_OCAML_BYTES(x290);
   EverCrypt_Error_error_code x314 =
   EverCrypt_AEAD_decrypt_expand_chacha20_poly1305(x299, x300, x301, 
                                                   x304, x305, x308, 
                                                   x309, x312, x313);
   return Integers_val_uint8(x314);
}
value _11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305_byte9(value* argv,
                                                                int argc)
{
   value x315 = argv[8];
   value x316 = argv[7];
   value x317 = argv[6];
   value x318 = argv[5];
   value x319 = argv[4];
   value x320 = argv[3];
   value x321 = argv[2];
   value x322 = argv[1];
   value x323 = argv[0];
   return
     _11_EverCrypt_AEAD_decrypt_expand_chacha20_poly1305(x323, x322, 
                                                         x321, x320, 
                                                         x319, x318, 
                                                         x317, x316, 
                                                         x315);
}
value _12_EverCrypt_AEAD_decrypt_expand(value x333, value x332, value x331,
                                        value x330, value x329, value x328,
                                        value x327, value x326, value x325,
                                        value x324)
{
   uint8_t x334 = Uint8_val(x333);
   unsigned char* x337 = CTYPES_PTR_OF_OCAML_BYTES(x332);
   unsigned char* x338 = CTYPES_PTR_OF_OCAML_BYTES(x331);
   uint32_t x339 = Uint32_val(x330);
   unsigned char* x342 = CTYPES_PTR_OF_OCAML_BYTES(x329);
   uint32_t x343 = Uint32_val(x328);
   unsigned char* x346 = CTYPES_PTR_OF_OCAML_BYTES(x327);
   uint32_t x347 = Uint32_val(x326);
   unsigned char* x350 = CTYPES_PTR_OF_OCAML_BYTES(x325);
   unsigned char* x351 = CTYPES_PTR_OF_OCAML_BYTES(x324);
   EverCrypt_Error_error_code x352 =
   EverCrypt_AEAD_decrypt_expand(x334, x337, x338, x339, x342, x343, 
                                 x346, x347, x350, x351);
   return Integers_val_uint8(x352);
}
value _12_EverCrypt_AEAD_decrypt_expand_byte10(value* argv, int argc)
{
   value x353 = argv[9];
   value x354 = argv[8];
   value x355 = argv[7];
   value x356 = argv[6];
   value x357 = argv[5];
   value x358 = argv[4];
   value x359 = argv[3];
   value x360 = argv[2];
   value x361 = argv[1];
   value x362 = argv[0];
   return
     _12_EverCrypt_AEAD_decrypt_expand(x362, x361, x360, x359, x358, 
                                       x357, x356, x355, x354, x353);
}
value _13_EverCrypt_AEAD_free(value x363)
{
   struct EverCrypt_AEAD_state_s_s* x364 = CTYPES_ADDR_OF_FATPTR(x363);
   EverCrypt_AEAD_free(x364);
   return Val_unit;
}
