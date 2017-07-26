module type S = sig
  type t

  val to_string_monoid : t -> String_monoid.t
end
