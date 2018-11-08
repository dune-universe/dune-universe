type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type S = sig
  type t = Optint.t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val default : t
  val digest_bytes : Bytes.t -> int -> int -> t -> t
  val digest_string : String.t -> int -> int -> t -> t
  val digest_bigstring : bigstring -> int -> int -> t -> t
end

module Adler32 : S = struct
  type t = Optint.t

  let equal = Optint.equal
  let pp = Optint.pp
  let default = Optint.one
  let digest_bytes = Gin_adler32.digest_bytes
  let digest_string = Gin_adler32.digest_string
  let digest_bigstring = Gin_adler32.digest_bigstring
end

module Crc32c : S = struct
  type t = Optint.t

  let equal = Optint.equal
  let pp = Optint.pp
  let default = Optint.zero
  let digest_bytes = Gin_crc32c.digest_bytes
  let digest_string = Gin_crc32c.digest_string
  let digest_bigstring = Gin_crc32c.digest_bigstring
end

module Crc32 : S = struct
  type t = Optint.t

  let equal = Optint.equal
  let pp = Optint.pp
  let default = Optint.zero
  let digest_bytes = Gin_crc32.digest_bytes
  let digest_string = Gin_crc32.digest_string
  let digest_bigstring = Gin_crc32.digest_bigstring
end
