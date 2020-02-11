type expr =
  [%import: Expr.expr [@with int := int[@opaque]]]
  [@@deriving visitors { variety = "iter" }]
