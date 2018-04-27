module type S0 = sig
  type elt
  type t

  val scan_right: t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a C.t
  val scan_right_i: t -> init:'a -> f:(i:int -> elt -> 'a -> 'a) -> 'a C.t
  val scan_right_acc: acc:'acc -> t -> init:'a -> f:(acc:'acc -> elt -> 'a -> 'acc * 'a) -> 'a C.t
end

module type S1 = sig
  type 'a t

  val scan_right: 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b C.t
  val scan_right_i: 'a t -> init:'b -> f:(i:int -> 'a -> 'b -> 'b) -> 'b C.t
  val scan_right_acc: acc:'acc -> 'a t -> init:'b -> f:(acc:'acc -> 'a -> 'b -> 'acc * 'b) -> 'b C.t
end
