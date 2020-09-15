(** Mostly cargo-culted from https://github.com/edoliberty/streaming-quantiles *)

module type S = sig
  type elt

  type t

  val create :
    ?k:int -> ?c:float -> ?lazy_mode:bool -> ?alternate:bool -> unit -> t

  val update : t -> elt -> unit

  val update_n : t -> elt -> int -> unit

  val cdf : t -> (elt * float) array

  val pp_cdf : elt Fmt.t -> Format.formatter -> (elt * float) array -> unit
end

module Make (T : Set.OrderedType) : S with type elt := T.t
