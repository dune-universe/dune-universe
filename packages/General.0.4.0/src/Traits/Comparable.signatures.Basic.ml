module type S0 = sig
  type t

  val compare: t -> t
    -> Compare.t
end

module type S1 = sig
  type 'a t

  val compare: 'a t -> 'a t
    -> compare_a:('a -> 'a -> Compare.t)
    -> Compare.t
end

module type S2 = sig
  type ('a, 'b) t

  val compare: ('a, 'b) t -> ('a, 'b) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> Compare.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val compare: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> Compare.t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val compare: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> Compare.t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val compare: ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) t
    -> compare_a:('a -> 'a -> Compare.t)
    -> compare_b:('b -> 'b -> Compare.t)
    -> compare_c:('c -> 'c -> Compare.t)
    -> compare_d:('d -> 'd -> Compare.t)
    -> compare_e:('e -> 'e -> Compare.t)
    -> Compare.t
end
