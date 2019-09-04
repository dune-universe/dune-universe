module type S0 = sig
  type t

  val equal: t -> t
    -> bool
end

module type S1 = sig
  type 'a t

  val equal: 'a t -> 'a t
    -> equal_a:('a -> 'a -> bool)
    -> bool
end

module type S2 = sig
  type ('a, 'b) t

  val equal: ('a, 'b) t -> ('a, 'b) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> bool
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val equal: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> bool
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val equal: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> equal_d:('d -> 'd -> bool)
    -> bool
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val equal: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> equal_d:('d -> 'd -> bool)
    -> equal_e:('e -> 'e -> bool)
    -> bool
end
