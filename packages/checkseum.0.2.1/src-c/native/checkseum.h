#if !defined(H__CHECKSUM)
#define H__CHECKSUM

#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* XXX(dinosaure): snippet from digestif (<3 david). */

#define _ba_uint8_off(ba, off) ((uint8_t*) Caml_ba_data_val (ba) + Long_val (off))
#define _ba_uint32_off(ba, off) ((uint32_t*) Caml_ba_data_val (ba) + Long_val (off))
#define _ba_ulong_off(ba, off) ((u_long*) Caml_ba_data_val (ba) + Long_val (off))

#define _st_uint8_off(st, off) ((uint8_t*) String_val (st) + Long_val (off))
#define _st_uint32_off(st, off) ((uint32_t*) String_val (st) + Long_val (off))
#define _st_ulong_off(st, off) ((u_long*) String_val (st) + Long_val (off))

#define _ba_uint8(ba) _ba_uint8_off (ba, 0)
#define _ba_uint32(ba) _ba_uint32_off (ba, 0)
#define _ba_ulong(ba) _ba_ulong_off (ba, 0)

#define _st_uint8(st) _st_uint8_off(st, 0)
#define _st_uint32(st) _st_uint32_off(st, 0)
#define _st_ulong(st) _st_ulong_off (st, 0)

#define _ba_uint8_option_off(ba, off) (Is_block(ba) ? _ba_uint8_off(Field(ba, 0), off) : 0)
#define _ba_uint8_option(ba)          _ba_uint8_option_off (ba, 0)

#define _st_uint8_option_off(st, off) (Is_block(st) ? _st_uint8_off(Field(st, 0), off) : 0)
#define _st_uint8_option(st)          _ba_uint8_option_off (st, 0)

#endif /* H__CHECKSUM */
