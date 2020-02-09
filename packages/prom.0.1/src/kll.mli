module type S = sig
  type elt
  type t

  val create :
    ?k:int -> ?c:float -> ?lazy_mode:bool -> ?alternate:bool -> unit -> t

  val update : t -> elt -> unit
  val cdf : t -> (elt * float) list
  val pp_cdf : elt Fmt.t -> Format.formatter -> t -> unit
end

module Make(T:Set.OrderedType) : S with type elt := T.t
