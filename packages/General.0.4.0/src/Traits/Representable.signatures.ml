module type S0 = sig
  type t

  val repr: t
    -> string
end

module type S1 = sig
  type 'a t

  val repr: 'a t
    -> repr_a:('a -> string)
    -> string
end

module type S2 = sig
  type ('a, 'b) t

  val repr: ('a, 'b) t
    -> repr_a:('a -> string)
    -> repr_b:('b -> string)
    -> string
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val repr: ('a, 'b, 'c) t
    -> repr_a:('a -> string)
    -> repr_b:('b -> string)
    -> repr_c:('c -> string)
    -> string
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  val repr: ('a, 'b, 'c, 'd) t
    -> repr_a:('a -> string)
    -> repr_b:('b -> string)
    -> repr_c:('c -> string)
    -> repr_d:('d -> string)
    -> string
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  val repr: ('a, 'b, 'c, 'd, 'e) t
    -> repr_a:('a -> string)
    -> repr_b:('b -> string)
    -> repr_c:('c -> string)
    -> repr_d:('d -> string)
    -> repr_e:('e -> string)
    -> string
end
