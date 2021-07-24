
#include "Hacl_Streaming_MD5.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_MD5_legacy_create_in_md5(value x1)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x2 =
   Hacl_Streaming_MD5_legacy_create_in_md5();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_Streaming_MD5_legacy_init_md5(value x3)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x4 =
   CTYPES_ADDR_OF_FATPTR(x3);
   Hacl_Streaming_MD5_legacy_init_md5(x4);
   return Val_unit;
}
value _3_Hacl_Streaming_MD5_legacy_update_md5(value x8, value x7, value x6)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x9 =
   CTYPES_ADDR_OF_FATPTR(x8);
   unsigned char* x10 = CTYPES_PTR_OF_OCAML_BYTES(x7);
   uint32_t x11 = Uint32_val(x6);
   Hacl_Streaming_MD5_legacy_update_md5(x9, x10, x11);
   return Val_unit;
}
value _4_Hacl_Streaming_MD5_legacy_finish_md5(value x16, value x15)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x17 =
   CTYPES_ADDR_OF_FATPTR(x16);
   unsigned char* x18 = CTYPES_PTR_OF_OCAML_BYTES(x15);
   Hacl_Streaming_MD5_legacy_finish_md5(x17, x18);
   return Val_unit;
}
value _5_Hacl_Streaming_MD5_legacy_free_md5(value x20)
{
   struct Hacl_Streaming_SHA2_state_sha2_224_s* x21 =
   CTYPES_ADDR_OF_FATPTR(x20);
   Hacl_Streaming_MD5_legacy_free_md5(x21);
   return Val_unit;
}
