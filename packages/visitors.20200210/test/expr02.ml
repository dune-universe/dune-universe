type expr =
  | EConst of int
  | EAdd of expr * expr
  [@@deriving visitors { variety = "iter2"; concrete = true }]
