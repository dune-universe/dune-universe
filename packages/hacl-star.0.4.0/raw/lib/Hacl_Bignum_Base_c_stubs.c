
#include "Hacl_Bignum_Base.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Bignum_Base_mul_wide_add_u64(value x4, value x3, value x2,
                                           value x1)
{
   uint64_t x5 = Uint64_val(x4);
   uint64_t x8 = Uint64_val(x3);
   uint64_t x11 = Uint64_val(x2);
   uint64_t* x14 = CTYPES_ADDR_OF_FATPTR(x1);
   uint64_t x15 = Hacl_Bignum_Base_mul_wide_add_u64(x5, x8, x11, x14);
   return integers_copy_uint64(x15);
}
value _2_Hacl_Bignum_Base_mul_wide_add2_u32(value x19, value x18, value x17,
                                            value x16)
{
   uint32_t x20 = Uint32_val(x19);
   uint32_t x23 = Uint32_val(x18);
   uint32_t x26 = Uint32_val(x17);
   uint32_t* x29 = CTYPES_ADDR_OF_FATPTR(x16);
   uint32_t x30 = Hacl_Bignum_Base_mul_wide_add2_u32(x20, x23, x26, x29);
   return integers_copy_uint32(x30);
}
value _3_Hacl_Bignum_Base_mul_wide_add2_u64(value x34, value x33, value x32,
                                            value x31)
{
   uint64_t x35 = Uint64_val(x34);
   uint64_t x38 = Uint64_val(x33);
   uint64_t x41 = Uint64_val(x32);
   uint64_t* x44 = CTYPES_ADDR_OF_FATPTR(x31);
   uint64_t x45 = Hacl_Bignum_Base_mul_wide_add2_u64(x35, x38, x41, x44);
   return integers_copy_uint64(x45);
}
