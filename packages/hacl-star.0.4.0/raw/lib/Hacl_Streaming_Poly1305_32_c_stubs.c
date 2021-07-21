
#include "Hacl_Streaming_Poly1305_32.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_Poly1305_32_create_in(value x1)
{
   unsigned char* x2 = CTYPES_PTR_OF_OCAML_BYTES(x1);
   struct Hacl_Streaming_Poly1305_32_poly1305_32_state_s* x3 =
   Hacl_Streaming_Poly1305_32_create_in(x2);
   return CTYPES_FROM_PTR(x3);
}
value _2_Hacl_Streaming_Poly1305_32_init(value x5, value x4)
{
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   struct Hacl_Streaming_Poly1305_32_poly1305_32_state_s* x7 =
   CTYPES_ADDR_OF_FATPTR(x4);
   Hacl_Streaming_Poly1305_32_init(x6, x7);
   return Val_unit;
}
value _3_Hacl_Streaming_Poly1305_32_update(value x11, value x10, value x9)
{
   struct Hacl_Streaming_Poly1305_32_poly1305_32_state_s* x12 =
   CTYPES_ADDR_OF_FATPTR(x11);
   unsigned char* x13 = CTYPES_PTR_OF_OCAML_BYTES(x10);
   uint32_t x14 = Uint32_val(x9);
   Hacl_Streaming_Poly1305_32_update(x12, x13, x14);
   return Val_unit;
}
value _4_Hacl_Streaming_Poly1305_32_finish(value x19, value x18)
{
   struct Hacl_Streaming_Poly1305_32_poly1305_32_state_s* x20 =
   CTYPES_ADDR_OF_FATPTR(x19);
   unsigned char* x21 = CTYPES_PTR_OF_OCAML_BYTES(x18);
   Hacl_Streaming_Poly1305_32_finish(x20, x21);
   return Val_unit;
}
value _5_Hacl_Streaming_Poly1305_32_free(value x23)
{
   struct Hacl_Streaming_Poly1305_32_poly1305_32_state_s* x24 =
   CTYPES_ADDR_OF_FATPTR(x23);
   Hacl_Streaming_Poly1305_32_free(x24);
   return Val_unit;
}
