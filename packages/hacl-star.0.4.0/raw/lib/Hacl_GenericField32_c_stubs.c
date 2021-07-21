
#include "Hacl_GenericField32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_GenericField32_field_modulus_check(value x2, value x1)
{
   uint32_t x3 = Uint32_val(x2);
   uint32_t* x6 = CTYPES_ADDR_OF_FATPTR(x1);
   _Bool x7 = Hacl_GenericField32_field_modulus_check(x3, x6);
   return Val_bool(x7);
}
value _2_Hacl_GenericField32_field_init(value x9, value x8)
{
   uint32_t x10 = Uint32_val(x9);
   uint32_t* x13 = CTYPES_ADDR_OF_FATPTR(x8);
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x14 =
   Hacl_GenericField32_field_init(x10, x13);
   return CTYPES_FROM_PTR(x14);
}
value _3_Hacl_GenericField32_field_free(value x15)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x16 =
   CTYPES_ADDR_OF_FATPTR(x15);
   Hacl_GenericField32_field_free(x16);
   return Val_unit;
}
value _4_Hacl_GenericField32_field_get_len(value x18)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x19 =
   CTYPES_ADDR_OF_FATPTR(x18);
   uint32_t x20 = Hacl_GenericField32_field_get_len(x19);
   return integers_copy_uint32(x20);
}
value _5_Hacl_GenericField32_to_field(value x23, value x22, value x21)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x24 =
   CTYPES_ADDR_OF_FATPTR(x23);
   uint32_t* x25 = CTYPES_ADDR_OF_FATPTR(x22);
   uint32_t* x26 = CTYPES_ADDR_OF_FATPTR(x21);
   Hacl_GenericField32_to_field(x24, x25, x26);
   return Val_unit;
}
value _6_Hacl_GenericField32_from_field(value x30, value x29, value x28)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x31 =
   CTYPES_ADDR_OF_FATPTR(x30);
   uint32_t* x32 = CTYPES_ADDR_OF_FATPTR(x29);
   uint32_t* x33 = CTYPES_ADDR_OF_FATPTR(x28);
   Hacl_GenericField32_from_field(x31, x32, x33);
   return Val_unit;
}
value _7_Hacl_GenericField32_add(value x38, value x37, value x36, value x35)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x39 =
   CTYPES_ADDR_OF_FATPTR(x38);
   uint32_t* x40 = CTYPES_ADDR_OF_FATPTR(x37);
   uint32_t* x41 = CTYPES_ADDR_OF_FATPTR(x36);
   uint32_t* x42 = CTYPES_ADDR_OF_FATPTR(x35);
   Hacl_GenericField32_add(x39, x40, x41, x42);
   return Val_unit;
}
value _8_Hacl_GenericField32_sub(value x47, value x46, value x45, value x44)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x48 =
   CTYPES_ADDR_OF_FATPTR(x47);
   uint32_t* x49 = CTYPES_ADDR_OF_FATPTR(x46);
   uint32_t* x50 = CTYPES_ADDR_OF_FATPTR(x45);
   uint32_t* x51 = CTYPES_ADDR_OF_FATPTR(x44);
   Hacl_GenericField32_sub(x48, x49, x50, x51);
   return Val_unit;
}
value _9_Hacl_GenericField32_mul(value x56, value x55, value x54, value x53)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x57 =
   CTYPES_ADDR_OF_FATPTR(x56);
   uint32_t* x58 = CTYPES_ADDR_OF_FATPTR(x55);
   uint32_t* x59 = CTYPES_ADDR_OF_FATPTR(x54);
   uint32_t* x60 = CTYPES_ADDR_OF_FATPTR(x53);
   Hacl_GenericField32_mul(x57, x58, x59, x60);
   return Val_unit;
}
value _10_Hacl_GenericField32_sqr(value x64, value x63, value x62)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x65 =
   CTYPES_ADDR_OF_FATPTR(x64);
   uint32_t* x66 = CTYPES_ADDR_OF_FATPTR(x63);
   uint32_t* x67 = CTYPES_ADDR_OF_FATPTR(x62);
   Hacl_GenericField32_sqr(x65, x66, x67);
   return Val_unit;
}
value _11_Hacl_GenericField32_one(value x70, value x69)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x71 =
   CTYPES_ADDR_OF_FATPTR(x70);
   uint32_t* x72 = CTYPES_ADDR_OF_FATPTR(x69);
   Hacl_GenericField32_one(x71, x72);
   return Val_unit;
}
value _12_Hacl_GenericField32_exp_consttime(value x78, value x77, value x76,
                                            value x75, value x74)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x79 =
   CTYPES_ADDR_OF_FATPTR(x78);
   uint32_t* x80 = CTYPES_ADDR_OF_FATPTR(x77);
   uint32_t x81 = Uint32_val(x76);
   uint32_t* x84 = CTYPES_ADDR_OF_FATPTR(x75);
   uint32_t* x85 = CTYPES_ADDR_OF_FATPTR(x74);
   Hacl_GenericField32_exp_consttime(x79, x80, x81, x84, x85);
   return Val_unit;
}
value _13_Hacl_GenericField32_exp_vartime(value x91, value x90, value x89,
                                          value x88, value x87)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x92 =
   CTYPES_ADDR_OF_FATPTR(x91);
   uint32_t* x93 = CTYPES_ADDR_OF_FATPTR(x90);
   uint32_t x94 = Uint32_val(x89);
   uint32_t* x97 = CTYPES_ADDR_OF_FATPTR(x88);
   uint32_t* x98 = CTYPES_ADDR_OF_FATPTR(x87);
   Hacl_GenericField32_exp_vartime(x92, x93, x94, x97, x98);
   return Val_unit;
}
value _14_Hacl_GenericField32_inverse(value x102, value x101, value x100)
{
   struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32_s* x103 =
   CTYPES_ADDR_OF_FATPTR(x102);
   uint32_t* x104 = CTYPES_ADDR_OF_FATPTR(x101);
   uint32_t* x105 = CTYPES_ADDR_OF_FATPTR(x100);
   Hacl_GenericField32_inverse(x103, x104, x105);
   return Val_unit;
}
