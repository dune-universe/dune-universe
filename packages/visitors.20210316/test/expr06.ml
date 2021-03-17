type unop =
  | UnaryMinus

and binop =
  | BinaryMinus
  | BinaryAdd
  | BinaryMul
  | BinaryDiv

and expr =
  | EConst of int
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr

[@@deriving visitors { variety = "iter" },
            visitors { variety = "map"  }]
