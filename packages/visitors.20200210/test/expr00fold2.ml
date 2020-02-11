type expr =
  | EConst of (int[@opaque])
  | EAdd of expr * expr
  [@@deriving visitors { variety = "fold2" }]
