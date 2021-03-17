type t =
  | A
  | B of u

and u =
  | C of t [@name "A"]
  [@@deriving visitors { variety = "iter" }]

(* Another example where two distinct types have a data constructor
   renamed [A]. This causes a name clash on the methods [visit_A]. *)
