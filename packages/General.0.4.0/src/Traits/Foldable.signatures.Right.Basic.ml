module type S0 = sig
  type elt
  type t

  val fold_right: t -> init:'b -> f:(elt -> 'b -> 'b) -> 'b
  val fold_right_i: t -> init:'b -> f:(i:int -> elt -> 'b -> 'b) -> 'b
  val fold_right_acc: acc:'acc -> t -> init:'b -> f:(acc:'acc -> elt -> 'b -> 'acc * 'b) -> 'b
end

module type S1 = sig
  type 'a t

  val fold_right: 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
  val fold_right_i: 'a t -> init:'b -> f:(i:int -> 'a -> 'b -> 'b) -> 'b
  val fold_right_acc: acc:'acc -> 'a t -> init:'b -> f:(acc:'acc -> 'a -> 'b -> 'acc * 'b) -> 'b
end
