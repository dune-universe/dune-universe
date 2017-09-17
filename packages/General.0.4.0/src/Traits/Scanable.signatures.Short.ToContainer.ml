module type S0 = sig
  type elt
  type t

  val scan_short: init:'a -> t -> f:('a -> elt -> Shorten.t * 'a) -> 'a C.t
  val scan_short_i: init:'a -> t -> f:(i:int -> 'a -> elt -> Shorten.t * 'a) -> 'a C.t
  val scan_short_acc: acc:'acc -> init:'a -> t -> f:(acc:'acc -> 'a -> elt -> 'acc * Shorten.t * 'a) -> 'a C.t
end

module type S1 = sig
  type 'a t

  val scan_short: init:'b -> 'a t -> f:('b -> 'a -> Shorten.t * 'b) -> 'b C.t
  val scan_short_i: init:'b -> 'a t -> f:(i:int -> 'b -> 'a -> Shorten.t * 'b) -> 'b C.t
  val scan_short_acc: acc:'acc -> init:'b -> 'a t -> f:(acc:'acc -> 'b -> 'a -> 'acc * Shorten.t * 'b) -> 'b C.t
end
