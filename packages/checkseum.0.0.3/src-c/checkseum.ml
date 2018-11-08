type t = Optint.t

type ba =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type st = Bytes.t
type off = int
type len = int

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external adler32_bigstring :
  t -> ba -> off -> len -> t
  = "caml_checkseum_adler32_ba"

external adler32_bytes :
  t -> st -> off -> len -> t
  = "caml_checkseum_adler32_st"

external crc32c_bigstring :
  t -> ba -> off -> len -> t
  = "caml_checkseum_crc32c_ba"

external crc32c_bytes : t -> st -> off -> len -> t = "caml_checkseum_crc32c_st"

external crc32_bigstring :
  t -> ba -> off -> len -> t
  = "caml_checkseum_crc32_ba"

external crc32_bytes : t -> st -> off -> len -> t = "caml_checkseum_crc32_st"

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
  let digest_bytes bytes off len adler32 = adler32_bytes adler32 bytes off len

  let digest_string string off len adler32 =
    digest_bytes (Bytes.unsafe_of_string string) off len adler32

  let digest_bigstring bigstring off len adler32 =
    adler32_bigstring adler32 bigstring off len
end

module Crc32c : S = struct
  type t = Optint.t

  let equal = Optint.equal
  let pp = Optint.pp
  let default = Optint.zero
  let digest_bytes bytes off len crc32c = crc32c_bytes crc32c bytes off len

  let digest_string string off len crc32c =
    digest_bytes (Bytes.unsafe_of_string string) off len crc32c

  let digest_bigstring bigstring off len crc32c =
    crc32c_bigstring crc32c bigstring off len
end

module Crc32 : S = struct
  type t = Optint.t

  let equal = Optint.equal
  let pp = Optint.pp
  let default = Optint.zero
  let digest_bytes bytes off len crc32 = crc32_bytes crc32 bytes off len

  let digest_string string off len crc32 =
    digest_bytes (Bytes.unsafe_of_string string) off len crc32

  let digest_bigstring bigstring off len crc32 =
    crc32_bigstring crc32 bigstring off len
end
