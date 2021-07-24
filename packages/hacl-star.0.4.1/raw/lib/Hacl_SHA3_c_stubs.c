
#include "Hacl_SHA3.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Impl_SHA3_rotl(value x2, value x1)
{
   uint64_t x3 = Uint64_val(x2);
   uint32_t x6 = Uint32_val(x1);
   uint64_t x9 = Hacl_Impl_SHA3_rotl(x3, x6);
   return integers_copy_uint64(x9);
}
value _2_Hacl_Impl_SHA3_state_permute(value x10)
{
   uint64_t* x11 = CTYPES_ADDR_OF_FATPTR(x10);
   Hacl_Impl_SHA3_state_permute(x11);
   return Val_unit;
}
value _3_Hacl_Impl_SHA3_loadState(value x15, value x14, value x13)
{
   uint32_t x16 = Uint32_val(x15);
   unsigned char* x19 = CTYPES_PTR_OF_OCAML_BYTES(x14);
   uint64_t* x20 = CTYPES_ADDR_OF_FATPTR(x13);
   Hacl_Impl_SHA3_loadState(x16, x19, x20);
   return Val_unit;
}
value _4_Hacl_Impl_SHA3_storeState(value x24, value x23, value x22)
{
   uint32_t x25 = Uint32_val(x24);
   uint64_t* x28 = CTYPES_ADDR_OF_FATPTR(x23);
   unsigned char* x29 = CTYPES_PTR_OF_OCAML_BYTES(x22);
   Hacl_Impl_SHA3_storeState(x25, x28, x29);
   return Val_unit;
}
value _5_Hacl_Impl_SHA3_absorb(value x35, value x34, value x33, value x32,
                               value x31)
{
   uint64_t* x36 = CTYPES_ADDR_OF_FATPTR(x35);
   uint32_t x37 = Uint32_val(x34);
   uint32_t x40 = Uint32_val(x33);
   unsigned char* x43 = CTYPES_PTR_OF_OCAML_BYTES(x32);
   uint8_t x44 = Uint8_val(x31);
   Hacl_Impl_SHA3_absorb(x36, x37, x40, x43, x44);
   return Val_unit;
}
value _6_Hacl_Impl_SHA3_squeeze(value x51, value x50, value x49, value x48)
{
   uint64_t* x52 = CTYPES_ADDR_OF_FATPTR(x51);
   uint32_t x53 = Uint32_val(x50);
   uint32_t x56 = Uint32_val(x49);
   unsigned char* x59 = CTYPES_PTR_OF_OCAML_BYTES(x48);
   Hacl_Impl_SHA3_squeeze(x52, x53, x56, x59);
   return Val_unit;
}
value _7_Hacl_Impl_SHA3_keccak(value x67, value x66, value x65, value x64,
                               value x63, value x62, value x61)
{
   uint32_t x68 = Uint32_val(x67);
   uint32_t x71 = Uint32_val(x66);
   uint32_t x74 = Uint32_val(x65);
   unsigned char* x77 = CTYPES_PTR_OF_OCAML_BYTES(x64);
   uint8_t x78 = Uint8_val(x63);
   uint32_t x81 = Uint32_val(x62);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x61);
   Hacl_Impl_SHA3_keccak(x68, x71, x74, x77, x78, x81, x84);
   return Val_unit;
}
value _7_Hacl_Impl_SHA3_keccak_byte7(value* argv, int argc)
{
   value x86 = argv[6];
   value x87 = argv[5];
   value x88 = argv[4];
   value x89 = argv[3];
   value x90 = argv[2];
   value x91 = argv[1];
   value x92 = argv[0];
   return _7_Hacl_Impl_SHA3_keccak(x92, x91, x90, x89, x88, x87, x86);
}
value _8_Hacl_SHA3_shake128_hacl(value x96, value x95, value x94, value x93)
{
   uint32_t x97 = Uint32_val(x96);
   unsigned char* x100 = CTYPES_PTR_OF_OCAML_BYTES(x95);
   uint32_t x101 = Uint32_val(x94);
   unsigned char* x104 = CTYPES_PTR_OF_OCAML_BYTES(x93);
   Hacl_SHA3_shake128_hacl(x97, x100, x101, x104);
   return Val_unit;
}
value _9_Hacl_SHA3_shake256_hacl(value x109, value x108, value x107,
                                 value x106)
{
   uint32_t x110 = Uint32_val(x109);
   unsigned char* x113 = CTYPES_PTR_OF_OCAML_BYTES(x108);
   uint32_t x114 = Uint32_val(x107);
   unsigned char* x117 = CTYPES_PTR_OF_OCAML_BYTES(x106);
   Hacl_SHA3_shake256_hacl(x110, x113, x114, x117);
   return Val_unit;
}
value _10_Hacl_SHA3_sha3_224(value x121, value x120, value x119)
{
   uint32_t x122 = Uint32_val(x121);
   unsigned char* x125 = CTYPES_PTR_OF_OCAML_BYTES(x120);
   unsigned char* x126 = CTYPES_PTR_OF_OCAML_BYTES(x119);
   Hacl_SHA3_sha3_224(x122, x125, x126);
   return Val_unit;
}
value _11_Hacl_SHA3_sha3_256(value x130, value x129, value x128)
{
   uint32_t x131 = Uint32_val(x130);
   unsigned char* x134 = CTYPES_PTR_OF_OCAML_BYTES(x129);
   unsigned char* x135 = CTYPES_PTR_OF_OCAML_BYTES(x128);
   Hacl_SHA3_sha3_256(x131, x134, x135);
   return Val_unit;
}
value _12_Hacl_SHA3_sha3_384(value x139, value x138, value x137)
{
   uint32_t x140 = Uint32_val(x139);
   unsigned char* x143 = CTYPES_PTR_OF_OCAML_BYTES(x138);
   unsigned char* x144 = CTYPES_PTR_OF_OCAML_BYTES(x137);
   Hacl_SHA3_sha3_384(x140, x143, x144);
   return Val_unit;
}
value _13_Hacl_SHA3_sha3_512(value x148, value x147, value x146)
{
   uint32_t x149 = Uint32_val(x148);
   unsigned char* x152 = CTYPES_PTR_OF_OCAML_BYTES(x147);
   unsigned char* x153 = CTYPES_PTR_OF_OCAML_BYTES(x146);
   Hacl_SHA3_sha3_512(x149, x152, x153);
   return Val_unit;
}
