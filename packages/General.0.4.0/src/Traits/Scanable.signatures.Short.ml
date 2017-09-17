module type S0 = sig
  type elt
  type t

  val scan_short: init:elt -> t -> f:(elt -> elt -> Shorten.t * elt) -> t
  val scan_short_i: init:elt -> t -> f:(i:int -> elt -> elt -> Shorten.t * elt) -> t
  val scan_short_acc: acc:'acc -> init:elt -> t -> f:(acc:'acc -> elt -> elt -> 'acc * Shorten.t * elt) -> t
end

module type S1 = sig
  type 'a t

  val scan_short: init:'b -> 'a t -> f:('b -> 'a -> Shorten.t * 'b) -> 'b t
  val scan_short_i: init:'b -> 'a t -> f:(i:int -> 'b -> 'a -> Shorten.t * 'b) -> 'b t
  val scan_short_acc: acc:'acc -> init:'b -> 'a t -> f:(acc:'acc -> 'b -> 'a -> 'acc * Shorten.t * 'b) -> 'b t
end
