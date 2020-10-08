module Expr : sig
    @type 'self t =
    [ | `Var   of string
      | `Const of int
      | `Binop of (int -> int -> int) * string * 'self * 'self
    ]

  end
