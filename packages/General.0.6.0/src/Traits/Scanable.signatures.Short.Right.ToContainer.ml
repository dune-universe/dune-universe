module type S0 = sig
  type elt
  type t

  val scan_short_right: t -> init:'a -> f:(elt -> 'a -> Shorten.t * 'a) -> 'a C.t
  val scan_short_right_i: t -> init:'a -> f:(i:int -> elt -> 'a -> Shorten.t * 'a) -> 'a C.t
  val scan_short_right_acc: acc:'acc -> t -> init:'a -> f:(acc:'acc -> elt -> 'a -> 'acc * Shorten.t * 'a) -> 'a C.t
end

module type S1 = sig
  type 'a t

  val scan_short_right: 'a t -> init:'b -> f:('a -> 'b -> Shorten.t * 'b) -> 'b C.t
  val scan_short_right_i: 'a t -> init:'b -> f:(i:int -> 'a -> 'b -> Shorten.t * 'b) -> 'b C.t
  val scan_short_right_acc: acc:'acc -> 'a t -> init:'b -> f:(acc:'acc -> 'a -> 'b -> 'acc * Shorten.t * 'b) -> 'b C.t
end
