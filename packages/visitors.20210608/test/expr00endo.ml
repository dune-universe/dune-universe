type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { variety = "endo" }]
