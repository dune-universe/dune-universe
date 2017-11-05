open Stdint
open Fake_uint16

val crc_ccitt_generic        : input:string -> start_val:fuint16 -> fuint16

val crc_ccitt_generic_uint16 : input:string -> start_val:uint16  -> uint16
