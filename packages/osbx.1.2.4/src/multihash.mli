exception Length_mismatch
exception Invalid_hash_type_string

type hash_type  = [ `SHA1
                  | `SHA2_256     | `SHA256
                  | `SHA2_512_256
                  | `SHA2_512_512 | `SHA512
                  | `BLAKE2B_256
                  | `BLAKE2B_512
                  | `BLAKE2S_128
                  | `BLAKE2S_256
                  ]

type hash_bytes

module Specs : sig
  type param = { hash_func_type : string
               ; digest_length  : int
               }

  val hash_type_to_param          : hash_type -> param

  val hash_type_to_hash_func_type : hash_type -> string

  val hash_type_to_digest_length  : hash_type -> int

  val hash_type_to_total_length   : hash_type -> int
end

module Parser : sig
  val gen_parser  : hash_type -> hash_bytes Angstrom.t

  val all_parsers : hash_bytes Angstrom.t list
end

module Hash : sig
  exception Unsupported_hash

  type ctx

  val ctx_to_hash_type       : ctx       -> hash_type

  val hash_type_is_supported : hash_type -> bool

  val init                   : hash_type -> ctx

  val feed                   : ctx       -> string     -> unit

  val get_raw_hash           : ctx       -> string

  val get_hash_bytes         : ctx       -> hash_bytes
end

val all_hash_types   : hash_type list

val hash_bytes_equal : hash_bytes -> hash_bytes -> bool

val hash_type_to_string : hash_type -> string

val string_to_hash_type : string    -> (hash_type, string) result

val string_to_hash_type_exn : string -> hash_type

val raw_hash_to_hash_bytes  : hash_type   -> string -> hash_bytes

val hash_bytes_to_raw_hash  : hash_bytes -> string

val hash_bytes_to_multihash : hash_bytes -> string

val hash_bytes_to_hash_type : hash_bytes -> hash_type

val hash_bytes_to_hash_type_string : hash_bytes -> string

val raw_hash_to_multihash   : hash_type   -> string -> string

val make_dummy_hash_bytes   : hash_type   -> hash_bytes
