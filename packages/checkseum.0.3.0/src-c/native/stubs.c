#include "checkseum.h"
#include "adler32.h"
#include "crc32c.h"
#include "crc32.h"
#include "crc24.h"

#ifdef ARCH_SIXTYFOUR
/* XXX(dinosaure): un-allocated version for 64-bits architecture. */

#define __define_checkseum(name)                                                                \
	CAMLprim value                                                                          \
        caml_checkseum_ ## name ## _ba(value t, value src, value off, value len) {              \
          intnat res = checkseum_ ## name ## _digest (Int_val (t), _ba_uint8_off (src, off), Int_val (len)) ; \
	  return (Val_int (res)); \
	}                                                                                       \
                                                                                                \
        CAMLprim value                                                                          \
        caml_checkseum_ ## name ## _st(value t, value src, value off, value len) {              \
          intnat res = checkseum_ ## name ## _digest (Int_val (t), _st_uint8_off (src, off), Int_val (len)) ; \
	  return (Val_int (res)); \
        }

__define_checkseum (adler32)
__define_checkseum (crc32c)
__define_checkseum (crc32)
__define_checkseum (crc24)

#else
/* XXX(dinosaure): allocated version for 32-bits architecture. */

#define __define_checkseum(name)                                                                     \
	CAMLprim value                                                                               \
        caml_checkseum_ ## name ## _ba(value t, value src, value off, value len) {                   \
	  uint32_t res = checkseum_ ## name ## _digest (Int32_val (t), _ba_uint8_off (src, off), Int_val (len)) ; \
	  return (copy_int32 (res)); \
	}                                                                                            \
                                                                                                     \
        CAMLprim value                                                                               \
        caml_checkseum_ ## name ## _st(value t, value src, value off, value len) {                   \
	  uint32_t res = checkseum_ ## name ## _digest (Int32_val (t), _st_uint8_off (src, off), Int_val (len)) ; \
	  return (copy_int32 (res)); \
        }

__define_checkseum (adler32)
__define_checkseum (crc32c)
__define_checkseum (crc32)
__define_checkseum (crc24)

#endif
