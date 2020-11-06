(* This is the source calculus of the sample client. It is a core ML. *)

(* The terms carry no type annotations. *)

(* Nominal representation of term variables and binders. *)

type tevar = string
type term =
  | Var of tevar
  | Abs of tevar * term
  | App of term * term
  | Let of tevar * term * term
  | Tuple of term list
  | LetProd of tevar list * term * term
