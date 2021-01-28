type t =
  | Leaf
  | Node of { left: t; value: elt; right: t }
  [@@deriving visitors { variety = "iter" } ]

and elt = int[@@name "t"]

(*

In this example, a stupid [@name] attribute causes a name clash:
the types [elt] and [t] have visitor methods by the same name.

A warning should be issued.

*)
