open! Core

type t [@@deriving bin_io, sexp, compare, hash]

(* Case-insensitive. *)
module Domain : Mimestring.S with type t=string

val create : ?prefix:string -> ?domain:Domain.t -> string -> t

val of_string : ?default_domain:string -> string -> t Or_error.t
val of_string_exn : ?default_domain:string -> string -> t
val list_of_string : ?default_domain:string -> string -> t list Or_error.t
val list_of_string_exn : ?default_domain:string -> string -> t list
val to_string : t -> string
val list_to_header_value : t list -> string

val local_part : t -> string

val domain : t -> Domain.t option

(* local@domain, default brackets = false *)
val address_part
  : ?brackets:bool -> ?lowercase_domain:bool -> t -> t
val address_part_string
  : ?brackets:bool -> ?lowercase_domain:bool -> t -> string
(* Expects address part without brackets. *)
val set_address_part
  : t -> string -> t Or_error.t

(* Setting prefix to none removes the angular brackets. *)
val set_prefix : t -> string option -> t

val local_address : unit -> t

(* Hash and comparisons are based on the address part (local_part + domain)
   only. *)
include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t

module Caseless : sig
  type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
end
