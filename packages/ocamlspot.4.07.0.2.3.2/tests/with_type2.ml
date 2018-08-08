module type (* S => *) S (* <= S *) = sig
  type (* elt => *) elt (* <= elt *)
end
module X = struct
  type t
end

module type T = S (* ? S *) with type elt (* ? elt *) = X.t

module type F = functor( P : S ) -> S with type elt = P.elt
