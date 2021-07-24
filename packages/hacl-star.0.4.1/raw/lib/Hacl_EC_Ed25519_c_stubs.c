
#include "Hacl_EC_Ed25519.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_EC_Ed25519_mk_felem_zero(value x1)
{
   uint64_t* x2 = CTYPES_ADDR_OF_FATPTR(x1);
   Hacl_EC_Ed25519_mk_felem_zero(x2);
   return Val_unit;
}
value _2_Hacl_EC_Ed25519_mk_felem_one(value x4)
{
   uint64_t* x5 = CTYPES_ADDR_OF_FATPTR(x4);
   Hacl_EC_Ed25519_mk_felem_one(x5);
   return Val_unit;
}
value _3_Hacl_EC_Ed25519_felem_add(value x9, value x8, value x7)
{
   uint64_t* x10 = CTYPES_ADDR_OF_FATPTR(x9);
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x8);
   uint64_t* x12 = CTYPES_ADDR_OF_FATPTR(x7);
   Hacl_EC_Ed25519_felem_add(x10, x11, x12);
   return Val_unit;
}
value _4_Hacl_EC_Ed25519_felem_sub(value x16, value x15, value x14)
{
   uint64_t* x17 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t* x18 = CTYPES_ADDR_OF_FATPTR(x15);
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x14);
   Hacl_EC_Ed25519_felem_sub(x17, x18, x19);
   return Val_unit;
}
value _5_Hacl_EC_Ed25519_felem_mul(value x23, value x22, value x21)
{
   uint64_t* x24 = CTYPES_ADDR_OF_FATPTR(x23);
   uint64_t* x25 = CTYPES_ADDR_OF_FATPTR(x22);
   uint64_t* x26 = CTYPES_ADDR_OF_FATPTR(x21);
   Hacl_EC_Ed25519_felem_mul(x24, x25, x26);
   return Val_unit;
}
value _6_Hacl_EC_Ed25519_felem_inv(value x29, value x28)
{
   uint64_t* x30 = CTYPES_ADDR_OF_FATPTR(x29);
   uint64_t* x31 = CTYPES_ADDR_OF_FATPTR(x28);
   Hacl_EC_Ed25519_felem_inv(x30, x31);
   return Val_unit;
}
value _7_Hacl_EC_Ed25519_felem_load(value x34, value x33)
{
   unsigned char* x35 = CTYPES_PTR_OF_OCAML_BYTES(x34);
   uint64_t* x36 = CTYPES_ADDR_OF_FATPTR(x33);
   Hacl_EC_Ed25519_felem_load(x35, x36);
   return Val_unit;
}
value _8_Hacl_EC_Ed25519_felem_store(value x39, value x38)
{
   uint64_t* x40 = CTYPES_ADDR_OF_FATPTR(x39);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x38);
   Hacl_EC_Ed25519_felem_store(x40, x41);
   return Val_unit;
}
value _9_Hacl_EC_Ed25519_mk_point_at_inf(value x43)
{
   uint64_t* x44 = CTYPES_ADDR_OF_FATPTR(x43);
   Hacl_EC_Ed25519_mk_point_at_inf(x44);
   return Val_unit;
}
value _10_Hacl_EC_Ed25519_mk_base_point(value x46)
{
   uint64_t* x47 = CTYPES_ADDR_OF_FATPTR(x46);
   Hacl_EC_Ed25519_mk_base_point(x47);
   return Val_unit;
}
value _11_Hacl_EC_Ed25519_point_negate(value x50, value x49)
{
   uint64_t* x51 = CTYPES_ADDR_OF_FATPTR(x50);
   uint64_t* x52 = CTYPES_ADDR_OF_FATPTR(x49);
   Hacl_EC_Ed25519_point_negate(x51, x52);
   return Val_unit;
}
value _12_Hacl_EC_Ed25519_point_add(value x56, value x55, value x54)
{
   uint64_t* x57 = CTYPES_ADDR_OF_FATPTR(x56);
   uint64_t* x58 = CTYPES_ADDR_OF_FATPTR(x55);
   uint64_t* x59 = CTYPES_ADDR_OF_FATPTR(x54);
   Hacl_EC_Ed25519_point_add(x57, x58, x59);
   return Val_unit;
}
value _13_Hacl_EC_Ed25519_point_mul(value x63, value x62, value x61)
{
   unsigned char* x64 = CTYPES_PTR_OF_OCAML_BYTES(x63);
   uint64_t* x65 = CTYPES_ADDR_OF_FATPTR(x62);
   uint64_t* x66 = CTYPES_ADDR_OF_FATPTR(x61);
   Hacl_EC_Ed25519_point_mul(x64, x65, x66);
   return Val_unit;
}
value _14_Hacl_EC_Ed25519_point_eq(value x69, value x68)
{
   uint64_t* x70 = CTYPES_ADDR_OF_FATPTR(x69);
   uint64_t* x71 = CTYPES_ADDR_OF_FATPTR(x68);
   _Bool x72 = Hacl_EC_Ed25519_point_eq(x70, x71);
   return Val_bool(x72);
}
value _15_Hacl_EC_Ed25519_point_compress(value x74, value x73)
{
   uint64_t* x75 = CTYPES_ADDR_OF_FATPTR(x74);
   unsigned char* x76 = CTYPES_PTR_OF_OCAML_BYTES(x73);
   Hacl_EC_Ed25519_point_compress(x75, x76);
   return Val_unit;
}
value _16_Hacl_EC_Ed25519_point_decompress(value x79, value x78)
{
   unsigned char* x80 = CTYPES_PTR_OF_OCAML_BYTES(x79);
   uint64_t* x81 = CTYPES_ADDR_OF_FATPTR(x78);
   _Bool x82 = Hacl_EC_Ed25519_point_decompress(x80, x81);
   return Val_bool(x82);
}
