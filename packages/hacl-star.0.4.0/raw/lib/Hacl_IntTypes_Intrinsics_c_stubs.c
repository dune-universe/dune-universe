
#include "Hacl_IntTypes_Intrinsics.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_IntTypes_Intrinsics_add_carry_u32(value x4, value x3, value x2,
                                                value x1)
{
   uint32_t x5 = Uint32_val(x4);
   uint32_t x8 = Uint32_val(x3);
   uint32_t x11 = Uint32_val(x2);
   uint32_t* x14 = CTYPES_ADDR_OF_FATPTR(x1);
   uint32_t x15 = Hacl_IntTypes_Intrinsics_add_carry_u32(x5, x8, x11, x14);
   return integers_copy_uint32(x15);
}
value _2_Hacl_IntTypes_Intrinsics_add_carry_u64(value x19, value x18,
                                                value x17, value x16)
{
   uint64_t x20 = Uint64_val(x19);
   uint64_t x23 = Uint64_val(x18);
   uint64_t x26 = Uint64_val(x17);
   uint64_t* x29 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t x30 = Hacl_IntTypes_Intrinsics_add_carry_u64(x20, x23, x26, x29);
   return integers_copy_uint64(x30);
}
value _3_Hacl_IntTypes_Intrinsics_sub_borrow_u32(value x34, value x33,
                                                 value x32, value x31)
{
   uint32_t x35 = Uint32_val(x34);
   uint32_t x38 = Uint32_val(x33);
   uint32_t x41 = Uint32_val(x32);
   uint32_t* x44 = CTYPES_ADDR_OF_FATPTR(x31);
   uint32_t x45 =
   Hacl_IntTypes_Intrinsics_sub_borrow_u32(x35, x38, x41, x44);
   return integers_copy_uint32(x45);
}
value _4_Hacl_IntTypes_Intrinsics_sub_borrow_u64(value x49, value x48,
                                                 value x47, value x46)
{
   uint64_t x50 = Uint64_val(x49);
   uint64_t x53 = Uint64_val(x48);
   uint64_t x56 = Uint64_val(x47);
   uint64_t* x59 = CTYPES_ADDR_OF_FATPTR(x46);
   uint64_t x60 =
   Hacl_IntTypes_Intrinsics_sub_borrow_u64(x50, x53, x56, x59);
   return integers_copy_uint64(x60);
}
