type expr = Expr.expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { variety = "iter" }]
