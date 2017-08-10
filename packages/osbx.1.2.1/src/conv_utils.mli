open Stdint
open Nocrypto.Hash

type date_time_mode = [ `UTC | `Local ]

val uint64_to_bytes : uint64 -> bytes

val uint32_to_bytes : uint32 -> bytes

val uint16_to_bytes : uint16 -> bytes

val uint8_to_bytes  : uint8  -> bytes

val string_to_bytes : string -> bytes

val bytes_to_hex_string : bytes  -> string

val hex_string_to_bytes : string -> (bytes, string) result

val sha256_hash_state_to_bytes : SHA256.t -> bytes

val uint64_seconds_to_date_time_string : uint64 -> date_time_mode -> string
