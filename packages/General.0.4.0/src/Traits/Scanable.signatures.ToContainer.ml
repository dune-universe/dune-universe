module type S0 = sig
  type elt
  type t

  val scan: init:'a -> t -> f:('a -> elt -> 'a) -> 'a C.t
  val scan_i: init:'a -> t -> f:(i:int -> 'a -> elt -> 'a) -> 'a C.t
  val scan_acc: acc:'acc -> init:'a -> t -> f:(acc:'acc -> 'a -> elt -> 'acc * 'a) -> 'a C.t
end

module type S1 = sig
  type 'a t

  val scan: init:'b -> 'a t -> f:('b -> 'a -> 'b) -> 'b C.t
  val scan_i: init:'b -> 'a t -> f:(i:int -> 'b -> 'a -> 'b) -> 'b C.t
  val scan_acc: acc:'acc -> init:'b -> 'a t -> f:(acc:'acc -> 'b -> 'a -> 'acc * 'b) -> 'b C.t
end
