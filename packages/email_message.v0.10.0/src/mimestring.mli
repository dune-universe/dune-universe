open! Core_kernel

(* For usage in functors *)
module type S = sig
  type t [@@deriving sexp]
  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Case_insensitive : sig
  include S
    with type t = string
     and type comparator_witness = String.Caseless.comparator_witness

  val to_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
      include Stable
        with type t := t
         and type comparator_witness = String.Caseless.comparator_witness
    end
  end
end

val quote : string -> string
