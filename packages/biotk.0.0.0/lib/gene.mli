open Base

module Transcript : sig
  type t = private {
    id : string ;
    chr : string ;
    strand : [ `Plus | `Minus ] ;
    exons : (int * int) list ;
  }
  val range : t -> int * int
  val loc : t -> GLoc.t
  val exons : t -> GLoc.t list
  val introns : t -> GLoc.t list

  val upstream : t -> int -> GLoc.t
  (** Warning: not right-clipped (unknown chromosome length) *)

  val downstream : t -> int -> GLoc.t
  (** Warning: not right-clipped (unknown chromosome length) *)
end

type t = private {
  id : string ;
  chr : string ;
  strand : [ `Plus | `Minus ] ;
  transcripts : Transcript.t list ;
}

val make :
  id:string ->
  strand:[ `Plus | `Minus ] ->
  (string * GLoc.t list) list ->
  t Or_error.t

val range : t -> int * int
val loc : t -> GLoc.t

val exons : t -> GLoc.t list
val introns : t -> GLoc.t list

val upstream : t -> int -> GLoc.t
(** Warning: not right-clipped (unknown chromosome length) *)

val downstream : t -> int -> GLoc.t
(** Warning: not right-clipped (unknown chromosome length) *)
