
#include "Hacl_HMAC_DRBG.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HMAC_DRBG_reseed_interval(value _)
{
   uint32_t* x1 = &Hacl_HMAC_DRBG_reseed_interval;
   return CTYPES_FROM_PTR(x1);
}
value _2_Hacl_HMAC_DRBG_max_output_length(value _)
{
   uint32_t* x2 = &Hacl_HMAC_DRBG_max_output_length;
   return CTYPES_FROM_PTR(x2);
}
value _3_Hacl_HMAC_DRBG_max_length(value _)
{
   uint32_t* x3 = &Hacl_HMAC_DRBG_max_length;
   return CTYPES_FROM_PTR(x3);
}
value _4_Hacl_HMAC_DRBG_max_personalization_string_length(value _)
{
   uint32_t* x4 = &Hacl_HMAC_DRBG_max_personalization_string_length;
   return CTYPES_FROM_PTR(x4);
}
value _5_Hacl_HMAC_DRBG_max_additional_input_length(value _)
{
   uint32_t* x5 = &Hacl_HMAC_DRBG_max_additional_input_length;
   return CTYPES_FROM_PTR(x5);
}
value _6_Hacl_HMAC_DRBG_min_length(value x6)
{
   uint8_t x7 = Uint8_val(x6);
   uint32_t x10 = Hacl_HMAC_DRBG_min_length(x7);
   return integers_copy_uint32(x10);
}
value _7_Hacl_HMAC_DRBG_create_in(value x11)
{
   uint8_t x12 = Uint8_val(x11);
   struct Hacl_HMAC_DRBG_state_s x15 = Hacl_HMAC_DRBG_create_in(x12);
   return ctypes_copy_bytes(&x15, 24);
}
value _8_Hacl_HMAC_DRBG_instantiate(value x23, value x22, value x21,
                                    value x20, value x19, value x18,
                                    value x17, value x16)
{
   uint8_t x24 = Uint8_val(x23);
   void* x27 = CTYPES_ADDR_OF_FATPTR(x22);
   struct Hacl_HMAC_DRBG_state_s x29 = *(struct Hacl_HMAC_DRBG_state_s*)x27;
   uint32_t x30 = Uint32_val(x21);
   unsigned char* x33 = CTYPES_PTR_OF_OCAML_BYTES(x20);
   uint32_t x34 = Uint32_val(x19);
   unsigned char* x37 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   uint32_t x38 = Uint32_val(x17);
   unsigned char* x41 = CTYPES_PTR_OF_OCAML_BYTES(x16);
   Hacl_HMAC_DRBG_instantiate(x24, x29, x30, x33, x34, x37, x38, x41);
   return Val_unit;
}
value _8_Hacl_HMAC_DRBG_instantiate_byte8(value* argv, int argc)
{
   value x43 = argv[7];
   value x44 = argv[6];
   value x45 = argv[5];
   value x46 = argv[4];
   value x47 = argv[3];
   value x48 = argv[2];
   value x49 = argv[1];
   value x50 = argv[0];
   return
     _8_Hacl_HMAC_DRBG_instantiate(x50, x49, x48, x47, x46, x45, x44, x43);
}
value _9_Hacl_HMAC_DRBG_reseed(value x56, value x55, value x54, value x53,
                               value x52, value x51)
{
   uint8_t x57 = Uint8_val(x56);
   void* x60 = CTYPES_ADDR_OF_FATPTR(x55);
   struct Hacl_HMAC_DRBG_state_s x62 = *(struct Hacl_HMAC_DRBG_state_s*)x60;
   uint32_t x63 = Uint32_val(x54);
   unsigned char* x66 = CTYPES_PTR_OF_OCAML_BYTES(x53);
   uint32_t x67 = Uint32_val(x52);
   unsigned char* x70 = CTYPES_PTR_OF_OCAML_BYTES(x51);
   Hacl_HMAC_DRBG_reseed(x57, x62, x63, x66, x67, x70);
   return Val_unit;
}
value _9_Hacl_HMAC_DRBG_reseed_byte6(value* argv, int argc)
{
   value x72 = argv[5];
   value x73 = argv[4];
   value x74 = argv[3];
   value x75 = argv[2];
   value x76 = argv[1];
   value x77 = argv[0];
   return _9_Hacl_HMAC_DRBG_reseed(x77, x76, x75, x74, x73, x72);
}
value _10_Hacl_HMAC_DRBG_generate(value x83, value x82, value x81, value x80,
                                  value x79, value x78)
{
   uint8_t x84 = Uint8_val(x83);
   unsigned char* x87 = CTYPES_PTR_OF_OCAML_BYTES(x82);
   void* x88 = CTYPES_ADDR_OF_FATPTR(x81);
   struct Hacl_HMAC_DRBG_state_s x90 = *(struct Hacl_HMAC_DRBG_state_s*)x88;
   uint32_t x91 = Uint32_val(x80);
   uint32_t x94 = Uint32_val(x79);
   unsigned char* x97 = CTYPES_PTR_OF_OCAML_BYTES(x78);
   _Bool x98 = Hacl_HMAC_DRBG_generate(x84, x87, x90, x91, x94, x97);
   return Val_bool(x98);
}
value _10_Hacl_HMAC_DRBG_generate_byte6(value* argv, int argc)
{
   value x99 = argv[5];
   value x100 = argv[4];
   value x101 = argv[3];
   value x102 = argv[2];
   value x103 = argv[1];
   value x104 = argv[0];
   return _10_Hacl_HMAC_DRBG_generate(x104, x103, x102, x101, x100, x99);
}
