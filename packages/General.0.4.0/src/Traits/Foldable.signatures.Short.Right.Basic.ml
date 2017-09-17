module type S0 = sig
  type elt
  type t

  val fold_short_right: t -> init:'b -> f:(elt -> 'b -> Shorten.t * 'b) -> 'b
  val fold_short_right_i: t -> init:'b -> f:(i:int -> elt -> 'b -> Shorten.t * 'b) -> 'b
  val fold_short_right_acc: acc:'acc -> t -> init:'b -> f:(acc:'acc -> elt -> 'b -> 'acc * Shorten.t * 'b) -> 'b
end

module type S1 = sig
  type 'a t

  val fold_short_right: 'a t -> init:'b -> f:('a -> 'b -> Shorten.t * 'b) -> 'b
  val fold_short_right_i: 'a t -> init:'b -> f:(i:int -> 'a -> 'b -> Shorten.t * 'b) -> 'b
  val fold_short_right_acc: acc:'acc -> 'a t -> init:'b -> f:(acc:'acc -> 'a -> 'b -> 'acc * Shorten.t * 'b) -> 'b
end
