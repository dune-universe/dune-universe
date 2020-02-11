type expr =
  | EConst of int
  | EAdd of expr list
  [@@deriving visitors { variety = "iter" }]
