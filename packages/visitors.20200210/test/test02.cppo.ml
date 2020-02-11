type term =
  | TUnit
  | TIntLiteral of int
  | TVar of string
  | TLambda of string * term
  | TApp of term * term
#if OCAML_VERSION >= (4, 03, 0)
  | TPair of { fst: term; snd: term }
#endif
  | TTuple of term_list

and term_list =
  | TLNil
  | TLCons of (term * term_list)

[@@deriving
     visitors { variety = "iter"; concrete = true },
     visitors { variety = "map"; concrete = true },
     visitors { variety = "reduce"; concrete = true; ancestors = ["VisitorsRuntime.addition_monoid"] },
     visitors { variety = "endo"; concrete = true },
     visitors { variety = "iter2"; concrete = true },
     visitors { variety = "map2"; concrete = true },
     visitors { variety = "reduce2"; concrete = true; ancestors = ["VisitorsRuntime.addition_monoid"] }
]

let identity : term =
  TLambda ("x", TVar "x")

let () =
  new iter#visit_term 33 identity

let () =
  new iter#visit_term 33 (new map#visit_term () identity)
