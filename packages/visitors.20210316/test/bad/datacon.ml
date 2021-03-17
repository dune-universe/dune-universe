type t =
  | A
  | B of u

and u =
  | A of t
  [@@deriving visitors { variety = "iter" }]

(* Another example where two distinct types have a data constructor
   named [A] (which OCaml warns about, but allows). This causes a
   name clash on the methods [visit_A]. *)
