module type S0 = sig
  include Basic.S0

  val different: t -> t
    -> bool

  module O: Operators.S0 with type t := t
end

module type S1 = sig
  include Basic.S1

  val different: 'a t -> 'a t
    -> equal_a:('a -> 'a -> bool)
    -> bool
end

module type S2 = sig
  include Basic.S2

  val different: ('a, 'b) t -> ('a, 'b) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> bool
end

module type S3 = sig
  include Basic.S3

  val different: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> bool
end

module type S4 = sig
  include Basic.S4

  val different: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> equal_d:('d -> 'd -> bool)
    -> bool
end

module type S5 = sig
  include Basic.S5

  val different: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> equal_a:('a -> 'a -> bool)
    -> equal_b:('b -> 'b -> bool)
    -> equal_c:('c -> 'c -> bool)
    -> equal_d:('d -> 'd -> bool)
    -> equal_e:('e -> 'e -> bool)
    -> bool
end
