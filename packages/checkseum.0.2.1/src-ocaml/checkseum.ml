type bigstring =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

module type S = sig
  type t = Optint.t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val default : t

  val digest_bytes : Bytes.t -> int -> int -> t -> t

  val unsafe_digest_bytes : Bytes.t -> int -> int -> t -> t

  val digest_string : String.t -> int -> int -> t -> t

  val unsafe_digest_string : String.t -> int -> int -> t -> t

  val digest_bigstring : bigstring -> int -> int -> t -> t

  val unsafe_digest_bigstring : bigstring -> int -> int -> t -> t
end

module Adler32 : S = Gin_adler32

module Crc32c : S = Gin_crc32c

module Crc32 : S = Gin_crc32

module Crc24 : S = Gin_crc24
