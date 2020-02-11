type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr
  [@@deriving visitors { name = "omap"; variety = "map" }]
