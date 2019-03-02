open! Core

type t [@@deriving sexp_of]
type boundary = t

(** Creates a boundary from the value of the "boundary" parameter in a
    Content-type header (RFC2046, p.19)
*)

include Stringable.S with type t := t

module Generator : sig
  type t [@@deriving sexp_of]

  (** A boundary generator that should clash rarely. *)
  val default : t

  (** A generator using an existing boundary as a template. *)
  val from_existing_boundary : boundary -> t
end

(** Use the generator to find a boundary that doesn't conflict. *)
val generate_non_conflicting_boundary
  :  ?prologue:Bigstring_shared.t
  -> parts:String_monoid.t list
  -> ?epilogue:Bigstring_shared.t
  -> Generator.t
  -> t

(** Combine parts using the given boundary. This assumes that the boundary doesn't
    conflict. *)
val join_without_checking_for_conflicts
  :  ?prologue:Bigstring_shared.t
  -> parts:String_monoid.t list
  -> ?epilogue:Bigstring_shared.t
  -> t
  -> String_monoid.t

(** Splits a multipart body into a list of messages, and, if there are,
    an optional prologue and epilogue. *)
val split
  :  t
  -> Bigstring_shared.t
  -> Bigstring_shared.t option * Bigstring_shared.t list * Bigstring_shared.t option

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
