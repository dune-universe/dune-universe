module type S0 = sig
  type elt
  type t

  val fold_short: init:'b -> t -> f:('b -> elt -> Shorten.t * 'b) -> 'b
  val fold_short_i: init:'b -> t -> f:(i:int -> 'b -> elt -> Shorten.t * 'b) -> 'b
  val fold_short_acc: acc:'acc -> init:'b -> t -> f:(acc:'acc -> 'b -> elt -> 'acc * Shorten.t * 'b) -> 'b
end

module type S1 = sig
  type 'a t

  val fold_short: init:'b -> 'a t -> f:('b -> 'a -> Shorten.t * 'b) -> 'b
  val fold_short_i: init:'b -> 'a t -> f:(i:int -> 'b -> 'a -> Shorten.t * 'b) -> 'b
  val fold_short_acc: acc:'acc -> init:'b -> 'a t -> f:(acc:'acc -> 'b -> 'a -> 'acc * Shorten.t * 'b) -> 'b
end
