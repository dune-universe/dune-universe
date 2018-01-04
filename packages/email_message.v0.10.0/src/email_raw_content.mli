open! Core

type t [@@deriving compare, hash, sexp_of]

val to_bigstring_shared : t -> Bigstring_shared.t

val of_string           : string             -> t
val of_bigstring_shared : Bigstring_shared.t -> t

val length : t -> int

(** Even though the underlying type includes an option, most users should not have to
    think about the difference between [Some ""] and [None]. You can use the [Expert]
    module to deal with the optionality, if need be. *)
module Expert : sig
  val to_bigstring_shared_option : t -> Bigstring_shared.t option
  val of_bigstring_shared_option : Bigstring_shared.t option -> t
end

module Stable : sig
  module V1 : sig type nonrec t = t [@@deriving bin_io, sexp] end
end
