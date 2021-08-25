(** [linalg]: metaprogramming-friendly linear algebra *)

module Intf = Intf
module Vec = Vec
module Mat = Mat
module Tensor = Tensor

type ('s, 'i, 'e) vec = ('s, 'i, 'e) Intf.vec = Vec of 's * ('i -> 'e)

type ('s, 'i, 'e, 'w) ovec = ('s, 'i, 'e, 'w) Intf.ovec =
  | OVec of 's * ('i -> 'e -> 'w)
