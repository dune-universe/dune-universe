
#include "Hacl_Streaming_SHA2_256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_Streaming_SHA2_256_create_in(value x1)
{
   struct Hacl_Streaming_Functor_state_s___uint32_t_____s* x2 =
   Hacl_Streaming_SHA2_256_create_in();
   return CTYPES_FROM_PTR(x2);
}
value _2_Hacl_Streaming_SHA2_256_init(value x3)
{
   struct Hacl_Streaming_Functor_state_s___uint32_t_____s* x4 =
   CTYPES_ADDR_OF_FATPTR(x3);
   Hacl_Streaming_SHA2_256_init(x4);
   return Val_unit;
}
value _3_Hacl_Streaming_SHA2_256_update(value x8, value x7, value x6)
{
   struct Hacl_Streaming_Functor_state_s___uint32_t_____s* x9 =
   CTYPES_ADDR_OF_FATPTR(x8);
   char* x10 = CTYPES_PTR_OF_OCAML_STRING(x7);
   uint32_t x11 = Uint32_val(x6);
   Hacl_Streaming_SHA2_256_update(x9, x10, x11);
   return Val_unit;
}
value _4_Hacl_Streaming_SHA2_256_finish(value x16, value x15)
{
   struct Hacl_Streaming_Functor_state_s___uint32_t_____s* x17 =
   CTYPES_ADDR_OF_FATPTR(x16);
   char* x18 = CTYPES_PTR_OF_OCAML_STRING(x15);
   Hacl_Streaming_SHA2_256_finish(x17, x18);
   return Val_unit;
}
value _5_Hacl_Streaming_SHA2_256_free(value x20)
{
   struct Hacl_Streaming_Functor_state_s___uint32_t_____s* x21 =
   CTYPES_ADDR_OF_FATPTR(x20);
   Hacl_Streaming_SHA2_256_free(x21);
   return Val_unit;
}
