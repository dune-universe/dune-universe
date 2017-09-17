module type S0 = sig
  include Basic.S0

  val less_than: t -> t
    -> bool
  val less_or_equal: t -> t
    -> bool
  val greater_than: t -> t
    -> bool
  val greater_or_equal: t -> t
    -> bool

  val between: t -> low:t -> high:t
    -> bool
  val between_or_equal: t -> low:t -> high:t
    -> bool

  val min: t -> t
    -> t
  val max: t -> t
    -> t
  val min_max: t -> t
    -> t * t

  module O: Operators.S0 with type t := t
end

module type S1 = sig
  include Basic.S1

  val less_than: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool
  val less_or_equal: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool
  val greater_than: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool
  val greater_or_equal: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool

  val between: 'a t -> low:'a t -> high:'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool
  val between_or_equal: 'a t -> low:'a t -> high:'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> bool

  val min: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> 'a t
  val max: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> 'a t
  val min_max: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> 'a t * 'a t
end

module type S2 = sig
  include Basic.S2

  val less_than: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool
  val less_or_equal: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool
  val greater_than: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool
  val greater_or_equal: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool

  val between: ('a, 'b) t -> low:('a, 'b) t -> high:('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool
  val between_or_equal: ('a, 'b) t -> low:('a, 'b) t -> high:('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> bool

  val min: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> ('a, 'b) t
  val max: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> ('a, 'b) t
  val min_max: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> ('a, 'b) t * ('a, 'b) t
end

module type S3 = sig
  include Basic.S3

  val less_than: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool
  val less_or_equal: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool
  val greater_than: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool
  val greater_or_equal: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool

  val between: ('a, 'b, 'c) t -> low:('a, 'b, 'c) t -> high:('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool
  val between_or_equal: ('a, 'b, 'c) t -> low:('a, 'b, 'c) t -> high:('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> bool

  val min: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> ('a, 'b, 'c) t
  val max: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> ('a, 'b, 'c) t
  val min_max: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> ('a, 'b, 'c) t * ('a, 'b, 'c) t
end

module type S4 = sig
  include Basic.S4

  val less_than: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool
  val less_or_equal: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool
  val greater_than: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool
  val greater_or_equal: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool

  val between: ('a, 'b, 'c, 'd) t -> low:('a, 'b, 'c, 'd) t -> high:('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool
  val between_or_equal: ('a, 'b, 'c, 'd) t -> low:('a, 'b, 'c, 'd) t -> high:('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> bool

  val min: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> ('a, 'b, 'c, 'd) t
  val max: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> ('a, 'b, 'c, 'd) t
  val min_max: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> ('a, 'b, 'c, 'd) t * ('a, 'b, 'c, 'd) t
end

module type S5 = sig
  include Basic.S5

  val less_than: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool
  val less_or_equal: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool
  val greater_than: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool
  val greater_or_equal: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool

  val between: ('a, 'b, 'c, 'd, 'e) t -> low:('a, 'b, 'c, 'd, 'e) t -> high:('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool
  val between_or_equal: ('a, 'b, 'c, 'd, 'e) t -> low:('a, 'b, 'c, 'd, 'e) t -> high:('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> bool

  val min: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> ('a, 'b, 'c, 'd, 'e) t
  val max: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> ('a, 'b, 'c, 'd, 'e) t
  val min_max: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> ('a, 'b, 'c, 'd, 'e) t * ('a, 'b, 'c, 'd, 'e) t
end
