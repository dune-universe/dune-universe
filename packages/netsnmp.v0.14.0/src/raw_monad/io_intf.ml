module type S = sig
  type 'a t

  val wrap   : ('a -> 'b) -> 'a -> 'b t
  val wrap_mt : ('a -> 'b) -> 'a -> 'b t

  val bind   : 'a t -> f:('a -> 'b t) -> 'b t
  val map    : 'a t -> f:('a -> 'b) -> 'b t
  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|)  : 'a t -> ('a -> 'b) -> 'b t
  val gc_finalise : ('a -> unit t) -> 'a -> unit
end

