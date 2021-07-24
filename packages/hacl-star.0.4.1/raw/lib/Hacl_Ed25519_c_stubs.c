
#include "Hacl_Ed25519.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Bignum25519_reduce_513(value x1)
{
   uint64_t* x2 = CTYPES_ADDR_OF_FATPTR(x1);
   Hacl_Bignum25519_reduce_513(x2);
   return Val_unit;
}
value _2_Hacl_Bignum25519_inverse(value x5, value x4)
{
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x5);
   uint64_t* x7 = CTYPES_ADDR_OF_FATPTR(x4);
   Hacl_Bignum25519_inverse(x6, x7);
   return Val_unit;
}
value _3_Hacl_Bignum25519_load_51(value x10, value x9)
{
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x9);
   Hacl_Bignum25519_load_51(x11, x12);
   return Val_unit;
}
value _4_Hacl_Bignum25519_store_51(value x15, value x14)
{
   unsigned char* x16 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   uint64_t* x17 = CTYPES_ADDR_OF_FATPTR(x14);
   Hacl_Bignum25519_store_51(x16, x17);
   return Val_unit;
}
value _5_Hacl_Impl_Ed25519_PointAdd_point_add(value x21, value x20,
                                              value x19)
{
   uint64_t* x22 = CTYPES_ADDR_OF_FATPTR(x21);
   uint64_t* x23 = CTYPES_ADDR_OF_FATPTR(x20);
   uint64_t* x24 = CTYPES_ADDR_OF_FATPTR(x19);
   Hacl_Impl_Ed25519_PointAdd_point_add(x22, x23, x24);
   return Val_unit;
}
value _6_Hacl_Impl_Ed25519_Ladder_point_mul(value x28, value x27, value x26)
{
   uint64_t* x29 = CTYPES_ADDR_OF_FATPTR(x28);
   unsigned char* x30 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint64_t* x31 = CTYPES_ADDR_OF_FATPTR(x26);
   Hacl_Impl_Ed25519_Ladder_point_mul(x29, x30, x31);
   return Val_unit;
}
value _7_Hacl_Impl_Ed25519_PointCompress_point_compress(value x34, value x33)
{
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x34);
   uint64_t* x36 = CTYPES_ADDR_OF_FATPTR(x33);
   Hacl_Impl_Ed25519_PointCompress_point_compress(x35, x36);
   return Val_unit;
}
value _8_Hacl_Impl_Ed25519_PointDecompress_point_decompress(value x39,
                                                            value x38)
{
   uint64_t* x40 = CTYPES_ADDR_OF_FATPTR(x39);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x38);
   _Bool x42 = Hacl_Impl_Ed25519_PointDecompress_point_decompress(x40, x41);
   return Val_bool(x42);
}
value _9_Hacl_Impl_Ed25519_PointEqual_point_equal(value x44, value x43)
{
   uint64_t* x45 = CTYPES_ADDR_OF_FATPTR(x44);
   uint64_t* x46 = CTYPES_ADDR_OF_FATPTR(x43);
   _Bool x47 = Hacl_Impl_Ed25519_PointEqual_point_equal(x45, x46);
   return Val_bool(x47);
}
value _10_Hacl_Impl_Ed25519_PointNegate_point_negate(value x49, value x48)
{
   uint64_t* x50 = CTYPES_ADDR_OF_FATPTR(x49);
   uint64_t* x51 = CTYPES_ADDR_OF_FATPTR(x48);
   Hacl_Impl_Ed25519_PointNegate_point_negate(x50, x51);
   return Val_unit;
}
value _11_Hacl_Ed25519_sign(value x56, value x55, value x54, value x53)
{
   unsigned char* x57 = CTYPES_PTR_OF_OCAML_BYTES(x56);
   unsigned char* x58 = CTYPES_PTR_OF_OCAML_BYTES(x55);
   uint32_t x59 = Uint32_val(x54);
   unsigned char* x62 = CTYPES_PTR_OF_OCAML_BYTES(x53);
   Hacl_Ed25519_sign(x57, x58, x59, x62);
   return Val_unit;
}
value _12_Hacl_Ed25519_verify(value x67, value x66, value x65, value x64)
{
   unsigned char* x68 = CTYPES_PTR_OF_OCAML_BYTES(x67);
   uint32_t x69 = Uint32_val(x66);
   unsigned char* x72 = CTYPES_PTR_OF_OCAML_BYTES(x65);
   unsigned char* x73 = CTYPES_PTR_OF_OCAML_BYTES(x64);
   _Bool x74 = Hacl_Ed25519_verify(x68, x69, x72, x73);
   return Val_bool(x74);
}
value _13_Hacl_Ed25519_secret_to_public(value x76, value x75)
{
   unsigned char* x77 = CTYPES_PTR_OF_OCAML_BYTES(x76);
   unsigned char* x78 = CTYPES_PTR_OF_OCAML_BYTES(x75);
   Hacl_Ed25519_secret_to_public(x77, x78);
   return Val_unit;
}
value _14_Hacl_Ed25519_expand_keys(value x81, value x80)
{
   unsigned char* x82 = CTYPES_PTR_OF_OCAML_BYTES(x81);
   unsigned char* x83 = CTYPES_PTR_OF_OCAML_BYTES(x80);
   Hacl_Ed25519_expand_keys(x82, x83);
   return Val_unit;
}
value _15_Hacl_Ed25519_sign_expanded(value x88, value x87, value x86,
                                     value x85)
{
   unsigned char* x89 = CTYPES_PTR_OF_OCAML_BYTES(x88);
   unsigned char* x90 = CTYPES_PTR_OF_OCAML_BYTES(x87);
   uint32_t x91 = Uint32_val(x86);
   unsigned char* x94 = CTYPES_PTR_OF_OCAML_BYTES(x85);
   Hacl_Ed25519_sign_expanded(x89, x90, x91, x94);
   return Val_unit;
}
