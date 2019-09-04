module type S0 = sig
  include Basic.S0

  val reduce: t -> f:(elt -> elt -> elt) -> elt
  val reduce_i: t -> f:(i:int -> elt -> elt -> elt) -> elt
  val reduce_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * elt) -> elt

  val try_reduce: t -> f:(elt -> elt -> elt) -> (elt) option
  val try_reduce_i: t -> f:(i:int -> elt -> elt -> elt) -> (elt) option
  val try_reduce_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> elt -> 'acc * elt) -> (elt) option

  val iter: t -> f:(elt -> unit) -> unit
  val iter_i: t -> f:(i:int -> elt -> unit) -> unit
  val iter_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> 'acc) -> unit

  val count: t -> f:(elt -> bool) -> int
  val count_i: t -> f:(i:int -> elt -> bool) -> int
  val count_acc: acc:'acc -> t -> f:(acc:'acc -> elt -> 'acc * bool) -> int
end

module type S1 = sig
  include Basic.S1

  val reduce: 'a t -> f:('a -> 'a -> 'a) -> 'a
  val reduce_i: 'a t -> f:(i:int -> 'a -> 'a -> 'a) -> 'a
  val reduce_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * 'a) -> 'a

  val try_reduce: 'a t -> f:('a -> 'a -> 'a) -> ('a) option
  val try_reduce_i: 'a t -> f:(i:int -> 'a -> 'a -> 'a) -> ('a) option
  val try_reduce_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'a -> 'acc * 'a) -> ('a) option

  val iter: 'a t -> f:('a -> unit) -> unit
  val iter_i: 'a t -> f:(i:int -> 'a -> unit) -> unit
  val iter_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'acc) -> unit

  val count: 'a t -> f:('a -> bool) -> int
  val count_i: 'a t -> f:(i:int -> 'a -> bool) -> int
  val count_acc: acc:'acc -> 'a t -> f:(acc:'acc -> 'a -> 'acc * bool) -> int
end
