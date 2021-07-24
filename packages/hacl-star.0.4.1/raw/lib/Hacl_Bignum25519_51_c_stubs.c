
#include "Hacl_Bignum25519_51.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_Curve25519_Field51_fadd(value x3, value x2, value x1)
{
   uint64_t* x4 = CTYPES_ADDR_OF_FATPTR(x3);
   uint64_t* x5 = CTYPES_ADDR_OF_FATPTR(x2);
   uint64_t* x6 = CTYPES_ADDR_OF_FATPTR(x1);
   Hacl_Impl_Curve25519_Field51_fadd(x4, x5, x6);
   return Val_unit;
}
value _2_Hacl_Impl_Curve25519_Field51_fsub(value x10, value x9, value x8)
{
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   uint64_t* x12 = CTYPES_ADDR_OF_FATPTR(x9);
   uint64_t* x13 = CTYPES_ADDR_OF_FATPTR(x8);
   Hacl_Impl_Curve25519_Field51_fsub(x11, x12, x13);
   return Val_unit;
}
value _3_Hacl_Impl_Curve25519_Field51_fmul1(value x17, value x16, value x15)
{
   uint64_t* x18 = CTYPES_ADDR_OF_FATPTR(x17);
   uint64_t* x19 = CTYPES_ADDR_OF_FATPTR(x16);
   uint64_t x20 = Uint64_val(x15);
   Hacl_Impl_Curve25519_Field51_fmul1(x18, x19, x20);
   return Val_unit;
}
value _4_Hacl_Impl_Curve25519_Field51_store_felem(value x25, value x24)
{
   uint64_t* x26 = CTYPES_ADDR_OF_FATPTR(x25);
   uint64_t* x27 = CTYPES_ADDR_OF_FATPTR(x24);
   Hacl_Impl_Curve25519_Field51_store_felem(x26, x27);
   return Val_unit;
}
value _5_Hacl_Impl_Curve25519_Field51_cswap2(value x31, value x30, value x29)
{
   uint64_t x32 = Uint64_val(x31);
   uint64_t* x35 = CTYPES_ADDR_OF_FATPTR(x30);
   uint64_t* x36 = CTYPES_ADDR_OF_FATPTR(x29);
   Hacl_Impl_Curve25519_Field51_cswap2(x32, x35, x36);
   return Val_unit;
}
