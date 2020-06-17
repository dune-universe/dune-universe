type ba =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

type st = Bytes.t

type off = int

type len = int

type optint = Optint.t

(* XXX(dinosaure): we should be able to annot external with [@@noalloc] but
   it's depending on architecture and structural value of [Optint.t]. TODO! *)

module type FOREIGN = sig
  val unsafe_bytes : optint -> st -> off -> len -> optint

  val unsafe_bigstring : optint -> ba -> off -> len -> optint
end

module type DESC = sig
  val default : optint
end

module Adler32_foreign : FOREIGN = struct
  external unsafe_bytes : optint -> st -> off -> len -> optint
    = "caml_checkseum_adler32_st"

  external unsafe_bigstring : optint -> ba -> off -> len -> optint
    = "caml_checkseum_adler32_ba"
end

module Crc32_foreign : FOREIGN = struct
  external unsafe_bytes : optint -> st -> off -> len -> optint
    = "caml_checkseum_crc32_st"

  external unsafe_bigstring : optint -> ba -> off -> len -> optint
    = "caml_checkseum_crc32_ba"
end

module Crc32c_foreign : FOREIGN = struct
  external unsafe_bytes : optint -> st -> off -> len -> optint
    = "caml_checkseum_crc32c_st"

  external unsafe_bigstring : optint -> ba -> off -> len -> optint
    = "caml_checkseum_crc32c_ba"
end

module Crc24_foreign : FOREIGN = struct
  external unsafe_bytes : optint -> st -> off -> len -> optint
    = "caml_checkseum_crc24_st"

  external unsafe_bigstring : optint -> ba -> off -> len -> optint
    = "caml_checkseum_crc24_ba"
end

module Make (F : FOREIGN) (D : DESC) = struct
  type t = optint

  let pp ppf v = Optint.pp ppf v

  let equal a b = Optint.equal a b

  let default = D.default

  let unsafe_digest_bytes a o l v = F.unsafe_bytes v a o l

  let unsafe_digest_string a o l v =
    F.unsafe_bytes v (Bytes.unsafe_of_string a) o l

  let unsafe_digest_bigstring a o l v = F.unsafe_bigstring v a o l

  let digest_bytes a o l v =
    if o < 0 || l < 0 || o > Bytes.length a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_bytes a o l v

  let digest_string a o l v =
    if o < 0 || l < 0 || o > String.length a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_string a o l v

  let digest_bigstring a o l v =
    if o < 0 || l < 0 || o > Bigarray_compat.Array1.dim a - l
    then invalid_arg "index out of bounds" ;
    unsafe_digest_bigstring a o l v
end

type bigstring = ba

module type S = sig
  type t = optint

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

module Adler32 : S =
  Make
    (Adler32_foreign)
    (struct
      let default = Optint.one
    end)

module Crc32 : S =
  Make
    (Crc32_foreign)
    (struct
      let default = Optint.zero
    end)

module Crc32c : S =
  Make
    (Crc32c_foreign)
    (struct
      let default = Optint.zero
    end)

module Crc24 : S =
  Make
    (Crc24_foreign)
    (struct
      let default = Optint.of_int 0xb704ce
    end)
