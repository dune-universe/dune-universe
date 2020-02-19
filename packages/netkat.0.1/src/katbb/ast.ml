open Base

type test = { var: string; value: bool }
and act = test
and bexp = test Kat.Ast.bexp
and exp = (act, test) Kat.Ast.exp
  [@@deriving sexp, compare, equal, hash]
