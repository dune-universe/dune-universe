
#include "Hacl_IntTypes_Intrinsics.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_IntTypes_Intrinsics_add_carry_u64(value x4, value x3, value x2,
                                                value x1)
{
   uint64_t x5 = Uint64_val(x4);
   uint64_t x8 = Uint64_val(x3);
   uint64_t x11 = Uint64_val(x2);
   uint64_t* x14 = CTYPES_ADDR_OF_FATPTR(x1);
   uint64_t x15 = Hacl_IntTypes_Intrinsics_add_carry_u64(x5, x8, x11, x14);
   return integers_copy_uint64(x15);
}
value _2_Hacl_IntTypes_Intrinsics_sub_borrow_u64(value x19, value x18,
                                                 value x17, value x16)
{
   uint64_t x20 = Uint64_val(x19);
   uint64_t x23 = Uint64_val(x18);
   uint64_t x26 = Uint64_val(x17);
   uint64_t* x29 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t x30 =
   Hacl_IntTypes_Intrinsics_sub_borrow_u64(x20, x23, x26, x29);
   return integers_copy_uint64(x30);
}
