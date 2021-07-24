
#include "Hacl_HPKE_P256_CP32_SHA256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI(value x7, value x6, value x5,
                                               value x4, value x3, value x2,
                                               value x1)
{
   unsigned char* x8 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   unsigned char* x9 = CTYPES_PTR_OF_OCAML_BYTES(x6);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   unsigned char* x12 = CTYPES_PTR_OF_OCAML_BYTES(x3);
   uint32_t x13 = Uint32_val(x2);
   unsigned char* x16 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   uint32_t x17 =
   Hacl_HPKE_P256_CP32_SHA256_setupBaseI(x8, x9, x10, x11, x12, x13, x16);
   return integers_copy_uint32(x17);
}
value _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI_byte7(value* argv, int argc)
{
   value x18 = argv[6];
   value x19 = argv[5];
   value x20 = argv[4];
   value x21 = argv[3];
   value x22 = argv[2];
   value x23 = argv[1];
   value x24 = argv[0];
   return
     _1_Hacl_HPKE_P256_CP32_SHA256_setupBaseI(x24, x23, x22, x21, x20, 
                                              x19, x18);
}
value _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR(value x30, value x29,
                                               value x28, value x27,
                                               value x26, value x25)
{
   unsigned char* x31 = CTYPES_PTR_OF_OCAML_BYTES(x30);
   unsigned char* x32 = CTYPES_PTR_OF_OCAML_BYTES(x29);
   unsigned char* x33 = CTYPES_PTR_OF_OCAML_BYTES(x28);
   unsigned char* x34 = CTYPES_PTR_OF_OCAML_BYTES(x27);
   uint32_t x35 = Uint32_val(x26);
   unsigned char* x38 = CTYPES_PTR_OF_OCAML_BYTES(x25);
   uint32_t x39 =
   Hacl_HPKE_P256_CP32_SHA256_setupBaseR(x31, x32, x33, x34, x35, x38);
   return integers_copy_uint32(x39);
}
value _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR_byte6(value* argv, int argc)
{
   value x40 = argv[5];
   value x41 = argv[4];
   value x42 = argv[3];
   value x43 = argv[2];
   value x44 = argv[1];
   value x45 = argv[0];
   return
     _2_Hacl_HPKE_P256_CP32_SHA256_setupBaseR(x45, x44, x43, x42, x41, x40);
}
value _3_Hacl_HPKE_P256_CP32_SHA256_sealBase(value x52, value x51, value x50,
                                             value x49, value x48, value x47,
                                             value x46)
{
   unsigned char* x53 = CTYPES_PTR_OF_OCAML_BYTES(x52);
   unsigned char* x54 = CTYPES_PTR_OF_OCAML_BYTES(x51);
   uint32_t x55 = Uint32_val(x50);
   unsigned char* x58 = CTYPES_PTR_OF_OCAML_BYTES(x49);
   uint32_t x59 = Uint32_val(x48);
   unsigned char* x62 = CTYPES_PTR_OF_OCAML_BYTES(x47);
   unsigned char* x63 = CTYPES_PTR_OF_OCAML_BYTES(x46);
   uint32_t x64 =
   Hacl_HPKE_P256_CP32_SHA256_sealBase(x53, x54, x55, x58, x59, x62, x63);
   return integers_copy_uint32(x64);
}
value _3_Hacl_HPKE_P256_CP32_SHA256_sealBase_byte7(value* argv, int argc)
{
   value x65 = argv[6];
   value x66 = argv[5];
   value x67 = argv[4];
   value x68 = argv[3];
   value x69 = argv[2];
   value x70 = argv[1];
   value x71 = argv[0];
   return
     _3_Hacl_HPKE_P256_CP32_SHA256_sealBase(x71, x70, x69, x68, x67, 
                                            x66, x65);
}
value _4_Hacl_HPKE_P256_CP32_SHA256_openBase(value x78, value x77, value x76,
                                             value x75, value x74, value x73,
                                             value x72)
{
   unsigned char* x79 = CTYPES_PTR_OF_OCAML_BYTES(x78);
   unsigned char* x80 = CTYPES_PTR_OF_OCAML_BYTES(x77);
   uint32_t x81 = Uint32_val(x76);
   unsigned char* x84 = CTYPES_PTR_OF_OCAML_BYTES(x75);
   uint32_t x85 = Uint32_val(x74);
   unsigned char* x88 = CTYPES_PTR_OF_OCAML_BYTES(x73);
   unsigned char* x89 = CTYPES_PTR_OF_OCAML_BYTES(x72);
   uint32_t x90 =
   Hacl_HPKE_P256_CP32_SHA256_openBase(x79, x80, x81, x84, x85, x88, x89);
   return integers_copy_uint32(x90);
}
value _4_Hacl_HPKE_P256_CP32_SHA256_openBase_byte7(value* argv, int argc)
{
   value x91 = argv[6];
   value x92 = argv[5];
   value x93 = argv[4];
   value x94 = argv[3];
   value x95 = argv[2];
   value x96 = argv[1];
   value x97 = argv[0];
   return
     _4_Hacl_HPKE_P256_CP32_SHA256_openBase(x97, x96, x95, x94, x93, 
                                            x92, x91);
}
