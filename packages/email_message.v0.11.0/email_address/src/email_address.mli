open Core_kernel

module Domain : sig
  type t = String.Caseless.t [@@deriving sexp_of, compare]
  include Stringable with type t := t
  include Comparator.S
    with type t := t
     and type comparator_witness = String.Caseless.comparator_witness
end

type t [@@deriving sexp_of, compare, hash]

val create : ?prefix:string -> ?domain:Domain.t -> string -> t

val of_string : ?default_domain:string -> string -> t Or_error.t
val of_string_exn : ?default_domain:string -> string -> t
val list_of_string : ?default_domain:string -> string -> t list Or_error.t
val list_of_string_exn : ?default_domain:string -> string -> t list
val to_string : t -> string
val list_to_header_value : t list -> string

val local_part     : t -> string
val set_local_part : t -> string -> t

val domain     : t -> Domain.t option
val set_domain : t -> Domain.t option -> t

val address_part
  :  ?brackets:bool  (** default: [false] *)
  -> ?lowercase_domain :bool
  -> t
  -> t

val address_part_string
  :  ?brackets:bool  (** default: [false] *)
  -> ?lowercase_domain:bool
  -> t
  -> string

(** [set_address_part] expects an email address without prefix or angle brackets
    e.g. USER@DOMAIN. *)
val set_address_part : t -> string -> t Or_error.t

val prefix : t -> string option
(** [set_prefix] will remove angle brackets if given [None], otherwise angle brackets are
    added before the given prefix. *)
val set_prefix : t -> string option -> t

(* Hash and comparisons are based on the address part (local_part + domain)
   only. *)
include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Caseless : sig
  type nonrec t = t [@@deriving sexp_of, compare, hash]
  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Stable : sig
  module V1 : Stable_comparable.V1
    with type t = t
    with type comparator_witness = comparator_witness
end
