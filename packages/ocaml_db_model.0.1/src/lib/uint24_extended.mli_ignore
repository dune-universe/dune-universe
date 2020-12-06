module Uint24_extended :
  sig
    type t = Stdint.uint24
    val zero : t
    val one : t
    val max_int : t
    val min_int : t
    val bits : int
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t
    val succ : t -> t
    val pred : t -> t
    val abs : t -> t
    val neg : t -> t
    val logand : t -> t -> t
    val logor : t -> t -> t
    val logxor : t -> t -> t
    val lognot : t -> t
    val shift_left : t -> int -> t
    val shift_right : t -> int -> t
    val shift_right_logical : t -> int -> t
    val of_int : int -> t
    val to_int : t -> int
    val of_float : float -> t
    val to_float : t -> float
    val of_nativeint : nativeint -> t
    val to_nativeint : t -> nativeint
    val of_int8 : Stdint.int8 -> t
    val to_int8 : t -> Stdint.int8
    val of_int16 : Stdint.int16 -> t
    val to_int16 : t -> Stdint.int16
    val of_int24 : Stdint.int24 -> t
    val to_int24 : t -> Stdint.int24
    val of_int32 : Stdint.int32 -> t
    val to_int32 : t -> Stdint.int32
    val of_int40 : Stdint.int40 -> t
    val to_int40 : t -> Stdint.int40
    val of_int48 : Stdint.int48 -> t
    val to_int48 : t -> Stdint.int48
    val of_int56 : Stdint.int56 -> t
    val to_int56 : t -> Stdint.int56
    val of_int64 : Stdint.int64 -> t
    val to_int64 : t -> Stdint.int64
    val of_int128 : Stdint.int128 -> t
    val to_int128 : t -> Stdint.int128
    val of_uint8 : Stdint.uint8 -> t
    val to_uint8 : t -> Stdint.uint8
    val of_uint16 : Stdint.uint16 -> t
    val to_uint16 : t -> Stdint.uint16
    val of_uint24 : Stdint.uint24 -> t
    val to_uint24 : t -> Stdint.uint24
    val of_uint32 : Stdint.uint32 -> t
    val to_uint32 : t -> Stdint.uint32
    val of_uint40 : Stdint.uint40 -> t
    val to_uint40 : t -> Stdint.uint40
    val of_uint48 : Stdint.uint48 -> t
    val to_uint48 : t -> Stdint.uint48
    val of_uint56 : Stdint.uint56 -> t
    val to_uint56 : t -> Stdint.uint56
    val of_uint64 : Stdint.uint64 -> t
    val to_uint64 : t -> Stdint.uint64
    val of_uint128 : Stdint.uint128 -> t
    val to_uint128 : t -> Stdint.uint128
    val of_string : string -> t
    val to_string : t -> string
    val to_string_bin : t -> string
    val to_string_oct : t -> string
    val to_string_hex : t -> string
    val printer : Format.formatter -> t -> unit
    val printer_bin : Format.formatter -> t -> unit
    val printer_oct : Format.formatter -> t -> unit
    val printer_hex : Format.formatter -> t -> unit
    val of_bytes_big_endian : Bytes.t -> int -> t
    val of_bytes_little_endian : Bytes.t -> int -> t
    val to_bytes_big_endian : t -> Bytes.t -> int -> unit
    val to_bytes_little_endian : t -> Bytes.t -> int -> unit

    val pp : Format.formatter -> t -> unit
    val show : Stdint.Uint24.t -> string
    val pp_uint24 : Format.formatter -> t -> unit
    val show_uint24 : Stdint.Uint24.t -> string
    val equal_uint24 : Stdint.Uint24.t -> Stdint.Uint24.t -> bool
    val compare_uint24 : Stdint.Uint24.t -> Stdint.Uint24.t -> int
    val equal : Stdint.Uint24.t -> Stdint.Uint24.t -> bool
    val compare : Stdint.Uint24.t -> Stdint.Uint24.t -> int
    val to_yojson : Stdint.Uint24.t -> Yojson.Safe.json
    val of_yojson :
      Yojson.Safe.json -> (Stdint.Uint24.t, string) Core.Result.t
  end