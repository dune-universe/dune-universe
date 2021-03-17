type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { variety = "iter"; public = ["visit_expr"] }]
