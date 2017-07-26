open Core

(* Using [ {expr=42} ] as a representative expression suitable for [%sexp] *)

let _deprecated1 =
  [%structural_sexp {expr=42} ]

let _deprecated2 =
  [%structural_error "string-expr" {expr=42} ]

let _deprecated3 =
  [%raise_structural_sexp "string-expr" {expr=42} ]

let _deprecated4 =
  [%structural_or_error "string-expr" {expr=42} ]

let _deprecated5 ~x ~y ~z =
  [%sexp (x : int) (y + z : int) "literal"]
