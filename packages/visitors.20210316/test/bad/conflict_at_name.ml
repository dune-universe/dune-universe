module Elt = struct
  type elt = int
end

type t =
  | Leaf
  | Node of { left: t; value: (Elt.elt[@name "t"]); right: t }
  [@@deriving visitors { variety = "iter" } ]

(*

In this example, a stupid [@name] attribute causes a name clash:
the types [elt] and [t] have visitor methods by the same name.

A warning should be issued.

*)
