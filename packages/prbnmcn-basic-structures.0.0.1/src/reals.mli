(** Real numbers *)

(** Real numbers implemented as floating point numbers *)
module Float : Basic_intf.Reals with type t = float

(** Real numbers implemented as arbitrary-precision rational numbers *)
module Rational : Basic_intf.Reals with type t = Q.t
