module type S0 = sig
  include Basic.S0

  val reduce_short_right: t -> f:(elt -> elt -> Shorten.t * elt) -> elt
  val reduce_short_right_i: t -> f:(i:int -> elt -> elt -> Shorten.t * elt) -> elt
  val reduce_short_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * Shorten.t * elt) -> elt

  val try_reduce_short_right: t -> f:(elt -> elt -> Shorten.t * elt) -> (elt) option
  val try_reduce_short_right_i: t -> f:(i:int -> elt -> elt -> Shorten.t * elt) -> (elt) option
  val try_reduce_short_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * Shorten.t * elt) -> (elt) option

  val iter_short_right: t -> f:(elt -> Shorten.t) -> unit
  val iter_short_right_i: t -> f:(i:int -> elt -> Shorten.t) -> unit
  val iter_short_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> 'acc * Shorten.t) -> unit
end

module type S1 = sig
  include Basic.S1

  val reduce_short_right: 'a t -> f:('a -> 'a -> Shorten.t * 'a) -> 'a
  val reduce_short_right_i: 'a t -> f:(i:int -> 'a -> 'a -> Shorten.t * 'a) -> 'a
  val reduce_short_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * Shorten.t * 'a) -> 'a

  val try_reduce_short_right: 'a t -> f:('a -> 'a -> Shorten.t * 'a) -> ('a) option
  val try_reduce_short_right_i: 'a t -> f:(i:int -> 'a -> 'a -> Shorten.t * 'a) -> ('a) option
  val try_reduce_short_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * Shorten.t * 'a) -> ('a) option

  val iter_short_right: 'a t -> f:('a -> Shorten.t) -> unit
  val iter_short_right_i: 'a t -> f:(i:int -> 'a -> Shorten.t) -> unit
  val iter_short_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'acc * Shorten.t) -> unit
end
