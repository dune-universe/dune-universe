module type S0 = sig
  include Basic.S0

  val reduce_right: t -> f:(elt -> elt -> elt) -> elt
  val reduce_right_i: t -> f:(i:int -> elt -> elt -> elt) -> elt
  val reduce_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * elt) -> elt

  val try_reduce_right: t -> f:(elt -> elt -> elt) -> (elt) option
  val try_reduce_right_i: t -> f:(i:int -> elt -> elt -> elt) -> (elt) option
  val try_reduce_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * elt) -> (elt) option

  val iter_right: t -> f:(elt -> unit) -> unit
  val iter_right_i: t -> f:(i:int -> elt -> unit) -> unit
  val iter_right_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> 'acc * unit) -> unit
end

module type S1 = sig
  include Basic.S1

  val reduce_right: 'a t -> f:('a -> 'a -> 'a) -> 'a
  val reduce_right_i: 'a t -> f:(i:int -> 'a -> 'a -> 'a) -> 'a
  val reduce_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * 'a) -> 'a

  val try_reduce_right: 'a t -> f:('a -> 'a -> 'a) -> ('a) option
  val try_reduce_right_i: 'a t -> f:(i:int -> 'a -> 'a -> 'a) -> ('a) option
  val try_reduce_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * 'a) -> ('a) option

  val iter_right: 'a t -> f:('a -> unit) -> unit
  val iter_right_i: 'a t -> f:(i:int -> 'a -> unit) -> unit
  val iter_right_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'acc * unit) -> unit
end
