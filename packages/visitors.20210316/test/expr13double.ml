open Expr12
open Expr13

let double : expr -> expr =
  let v = object
    inherit [_] map
    method! visit_EConst _env k =
      EConst (2 * k)
  end in
  v # visit_'expr ()
