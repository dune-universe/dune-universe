open Ast_405

open Parsetree

type t =
  | End of expression
  | Bind of pattern option * expression * t * Location.t (* <-- *) * Location.t (* p <-- e *) * Location.t (* p <-- e; .. *)
  | Let of (expression -> expression) * t

val parse : expression option -> expression -> t

