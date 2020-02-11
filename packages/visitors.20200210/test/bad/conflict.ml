module Elt = struct
  type t = int
end

type t =
  | Leaf
  | Node of { left: t; value: Elt.t; right: t }
  [@@deriving visitors { variety = "iter" } ]

(*

Issue 3, reported by Gabriel Radanne.

https://gitlab.inria.fr/fpottier/visitors/issues/3

File "conflict.ml", line 5, characters 0-111:
Error: This expression has type Elt.t = int
       but an expression was expected of type t

The naming convention for visitor methods causes a name clash:
the types [Elt.t] and [t] have visitor methods by the same name.

A warning should be issued.

*)
