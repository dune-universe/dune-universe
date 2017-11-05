open Stdint
open Nocrypto.Hash

type date_time_mode = [ `UTC | `Local ]

type case = [ `Upper | `Lower ]

val uint64_to_string : uint64 -> string

val uint32_to_string : uint32 -> string

val uint16_to_string : uint16 -> string

val uint8_to_string  : uint8  -> string

val string_to_hex_string      : case:case -> string -> string

val string_to_hex_string_uid  : string    -> string

val string_to_hex_string_hash : string    -> string

val hex_string_to_string : string -> (string, string) result

(* val sha256_hash_state_to_bytes : SHA256.t -> bytes *)

val uint64_seconds_to_date_time_string : uint64 -> date_time_mode -> string
