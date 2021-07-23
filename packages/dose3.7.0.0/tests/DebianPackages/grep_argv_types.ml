type opt = Quiet | ShowField of string | NoFieldNames

type expr =
  | Field of (string * string)
  | And of (expr * expr)
  | Or of (expr * expr)
  | Not of expr
  | Exact of expr

type argv = Argv of (opt list * expr)
