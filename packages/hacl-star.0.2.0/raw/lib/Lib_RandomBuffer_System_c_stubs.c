
#include "Lib_RandomBuffer_System.h"
#include "ctypes_cstubs_internals.h"
value _1_Lib_RandomBuffer_System_randombytes(value x2, value x1)
{
   unsigned char* x3 = CTYPES_PTR_OF_OCAML_BYTES(x2);
   uint32_t x4 = Uint32_val(x1);
   _Bool x7 = Lib_RandomBuffer_System_randombytes(x3, x4);
   return Val_bool(x7);
}
