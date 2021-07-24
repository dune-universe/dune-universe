
#include "Hacl_P256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_P256_LowLevel_toUint8(value x2, value x1)
{
   uint64_t* x3 = CTYPES_ADDR_OF_FATPTR(x2);
   unsigned char* x4 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   Hacl_Impl_P256_LowLevel_toUint8(x3, x4);
   return Val_unit;
}
value _2_Hacl_Impl_P256_LowLevel_changeEndian(value x6)
{
   uint64_t* x7 = CTYPES_ADDR_OF_FATPTR(x6);
   Hacl_Impl_P256_LowLevel_changeEndian(x7);
   return Val_unit;
}
value _3_Hacl_Impl_P256_LowLevel_toUint64ChangeEndian(value x10, value x9)
{
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   uint64_t* x12 = CTYPES_ADDR_OF_FATPTR(x9);
   Hacl_Impl_P256_LowLevel_toUint64ChangeEndian(x11, x12);
   return Val_unit;
}
value _4_Hacl_Impl_P256_Core_isPointAtInfinityPrivate(value x14)
{
   uint64_t* x15 = CTYPES_ADDR_OF_FATPTR(x14);
   uint64_t x16 = Hacl_Impl_P256_Core_isPointAtInfinityPrivate(x15);
   return integers_copy_uint64(x16);
}
value _5_Hacl_Impl_P256_Core_secretToPublic(value x19, value x18, value x17)
{
   uint64_t* x20 = CTYPES_ADDR_OF_FATPTR(x19);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   uint64_t* x22 = CTYPES_ADDR_OF_FATPTR(x17);
   Hacl_Impl_P256_Core_secretToPublic(x20, x21, x22);
   return Val_unit;
}
value _6_Hacl_Impl_P256_DH__ecp256dh_r(value x26, value x25, value x24)
{
   uint64_t* x27 = CTYPES_ADDR_OF_FATPTR(x26);
   uint64_t* x28 = CTYPES_ADDR_OF_FATPTR(x25);
   unsigned char* x29 = CTYPES_PTR_OF_OCAML_BYTES(x24);
   uint64_t x30 = Hacl_Impl_P256_DH__ecp256dh_r(x27, x28, x29);
   return integers_copy_uint64(x30);
}
value _7_Hacl_P256_ecdsa_sign_p256_sha2(value x35, value x34, value x33,
                                        value x32, value x31)
{
   unsigned char* x36 = CTYPES_PTR_OF_OCAML_BYTES(x35);
   uint32_t x37 = Uint32_val(x34);
   unsigned char* x40 = CTYPES_PTR_OF_OCAML_BYTES(x33);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x32);
   unsigned char* x42 = CTYPES_PTR_OF_OCAML_BYTES(x31);
   _Bool x43 = Hacl_P256_ecdsa_sign_p256_sha2(x36, x37, x40, x41, x42);
   return Val_bool(x43);
}
value _8_Hacl_P256_ecdsa_sign_p256_sha384(value x48, value x47, value x46,
                                          value x45, value x44)
{
   unsigned char* x49 = CTYPES_PTR_OF_OCAML_BYTES(x48);
   uint32_t x50 = Uint32_val(x47);
   unsigned char* x53 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   unsigned char* x54 = CTYPES_PTR_OF_OCAML_BYTES(x45);
   unsigned char* x55 = CTYPES_PTR_OF_OCAML_BYTES(x44);
   _Bool x56 = Hacl_P256_ecdsa_sign_p256_sha384(x49, x50, x53, x54, x55);
   return Val_bool(x56);
}
value _9_Hacl_P256_ecdsa_sign_p256_sha512(value x61, value x60, value x59,
                                          value x58, value x57)
{
   unsigned char* x62 = CTYPES_PTR_OF_OCAML_BYTES(x61);
   uint32_t x63 = Uint32_val(x60);
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x59);
   unsigned char* x67 = CTYPES_PTR_OF_OCAML_BYTES(x58);
   unsigned char* x68 = CTYPES_PTR_OF_OCAML_BYTES(x57);
   _Bool x69 = Hacl_P256_ecdsa_sign_p256_sha512(x62, x63, x66, x67, x68);
   return Val_bool(x69);
}
value _10_Hacl_P256_ecdsa_sign_p256_without_hash(value x74, value x73,
                                                 value x72, value x71,
                                                 value x70)
{
   unsigned char* x75 = CTYPES_PTR_OF_OCAML_BYTES(x74);
   uint32_t x76 = Uint32_val(x73);
   unsigned char* x79 = CTYPES_PTR_OF_OCAML_BYTES(x72);
   unsigned char* x80 = CTYPES_PTR_OF_OCAML_BYTES(x71);
   unsigned char* x81 = CTYPES_PTR_OF_OCAML_BYTES(x70);
   _Bool x82 =
   Hacl_P256_ecdsa_sign_p256_without_hash(x75, x76, x79, x80, x81);
   return Val_bool(x82);
}
value _11_Hacl_P256_ecdsa_verif_p256_sha2(value x87, value x86, value x85,
                                          value x84, value x83)
{
   uint32_t x88 = Uint32_val(x87);
   unsigned char* x91 = CTYPES_PTR_OF_OCAML_BYTES(x86);
   unsigned char* x92 = CTYPES_PTR_OF_OCAML_BYTES(x85);
   unsigned char* x93 = CTYPES_PTR_OF_OCAML_BYTES(x84);
   unsigned char* x94 = CTYPES_PTR_OF_OCAML_BYTES(x83);
   _Bool x95 = Hacl_P256_ecdsa_verif_p256_sha2(x88, x91, x92, x93, x94);
   return Val_bool(x95);
}
value _12_Hacl_P256_ecdsa_verif_p256_sha384(value x100, value x99, value x98,
                                            value x97, value x96)
{
   uint32_t x101 = Uint32_val(x100);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x99);
   unsigned char* x105 = CTYPES_PTR_OF_OCAML_BYTES(x98);
   unsigned char* x106 = CTYPES_PTR_OF_OCAML_BYTES(x97);
   unsigned char* x107 = CTYPES_PTR_OF_OCAML_BYTES(x96);
   _Bool x108 =
   Hacl_P256_ecdsa_verif_p256_sha384(x101, x104, x105, x106, x107);
   return Val_bool(x108);
}
value _13_Hacl_P256_ecdsa_verif_p256_sha512(value x113, value x112,
                                            value x111, value x110,
                                            value x109)
{
   uint32_t x114 = Uint32_val(x113);
   unsigned char* x117 = CTYPES_PTR_OF_OCAML_BYTES(x112);
   unsigned char* x118 = CTYPES_PTR_OF_OCAML_BYTES(x111);
   unsigned char* x119 = CTYPES_PTR_OF_OCAML_BYTES(x110);
   unsigned char* x120 = CTYPES_PTR_OF_OCAML_BYTES(x109);
   _Bool x121 =
   Hacl_P256_ecdsa_verif_p256_sha512(x114, x117, x118, x119, x120);
   return Val_bool(x121);
}
value _14_Hacl_P256_ecdsa_verif_without_hash(value x126, value x125,
                                             value x124, value x123,
                                             value x122)
{
   uint32_t x127 = Uint32_val(x126);
   unsigned char* x130 = CTYPES_PTR_OF_OCAML_BYTES(x125);
   unsigned char* x131 = CTYPES_PTR_OF_OCAML_BYTES(x124);
   unsigned char* x132 = CTYPES_PTR_OF_OCAML_BYTES(x123);
   unsigned char* x133 = CTYPES_PTR_OF_OCAML_BYTES(x122);
   _Bool x134 =
   Hacl_P256_ecdsa_verif_without_hash(x127, x130, x131, x132, x133);
   return Val_bool(x134);
}
value _15_Hacl_P256_verify_q(value x135)
{
   unsigned char* x136 = CTYPES_PTR_OF_OCAML_BYTES(x135);
   _Bool x137 = Hacl_P256_verify_q(x136);
   return Val_bool(x137);
}
value _16_Hacl_P256_decompression_not_compressed_form(value x139, value x138)
{
   unsigned char* x140 = CTYPES_PTR_OF_OCAML_BYTES(x139);
   unsigned char* x141 = CTYPES_PTR_OF_OCAML_BYTES(x138);
   _Bool x142 = Hacl_P256_decompression_not_compressed_form(x140, x141);
   return Val_bool(x142);
}
value _17_Hacl_P256_decompression_compressed_form(value x144, value x143)
{
   unsigned char* x145 = CTYPES_PTR_OF_OCAML_BYTES(x144);
   unsigned char* x146 = CTYPES_PTR_OF_OCAML_BYTES(x143);
   _Bool x147 = Hacl_P256_decompression_compressed_form(x145, x146);
   return Val_bool(x147);
}
value _18_Hacl_P256_compression_not_compressed_form(value x149, value x148)
{
   unsigned char* x150 = CTYPES_PTR_OF_OCAML_BYTES(x149);
   unsigned char* x151 = CTYPES_PTR_OF_OCAML_BYTES(x148);
   Hacl_P256_compression_not_compressed_form(x150, x151);
   return Val_unit;
}
value _19_Hacl_P256_compression_compressed_form(value x154, value x153)
{
   unsigned char* x155 = CTYPES_PTR_OF_OCAML_BYTES(x154);
   unsigned char* x156 = CTYPES_PTR_OF_OCAML_BYTES(x153);
   Hacl_P256_compression_compressed_form(x155, x156);
   return Val_unit;
}
value _20_Hacl_P256_ecp256dh_i(value x159, value x158)
{
   unsigned char* x160 = CTYPES_PTR_OF_OCAML_BYTES(x159);
   unsigned char* x161 = CTYPES_PTR_OF_OCAML_BYTES(x158);
   _Bool x162 = Hacl_P256_ecp256dh_i(x160, x161);
   return Val_bool(x162);
}
value _21_Hacl_P256_ecp256dh_r(value x165, value x164, value x163)
{
   unsigned char* x166 = CTYPES_PTR_OF_OCAML_BYTES(x165);
   unsigned char* x167 = CTYPES_PTR_OF_OCAML_BYTES(x164);
   unsigned char* x168 = CTYPES_PTR_OF_OCAML_BYTES(x163);
   _Bool x169 = Hacl_P256_ecp256dh_r(x166, x167, x168);
   return Val_bool(x169);
}
value _22_Hacl_P256_is_more_than_zero_less_than_order(value x170)
{
   unsigned char* x171 = CTYPES_PTR_OF_OCAML_BYTES(x170);
   _Bool x172 = Hacl_P256_is_more_than_zero_less_than_order(x171);
   return Val_bool(x172);
}
