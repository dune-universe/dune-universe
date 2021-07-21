
#include "Hacl_HMAC_Blake2b_256.h"
#include "ctypes_cstubs_internals.h"
value _1_Hacl_HMAC_Blake2b_256_compute_blake2b_256(value x5, value x4,
                                                   value x3, value x2,
                                                   value x1)
{
   unsigned char* x6 = CTYPES_PTR_OF_OCAML_BYTES(x5);
   unsigned char* x7 = CTYPES_PTR_OF_OCAML_BYTES(x4);
   uint32_t x8 = Uint32_val(x3);
   unsigned char* x11 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x12 = Uint32_val(x1);
   Hacl_HMAC_Blake2b_256_compute_blake2b_256(x6, x7, x8, x11, x12);
   return Val_unit;
}
