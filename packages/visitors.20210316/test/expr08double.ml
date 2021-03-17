open Expr12 (* [oexpr] *)
open Expr08 (* [hexpr] *)

let double : hexpr -> hexpr =
  let v = object
    inherit [_] hmap
    method! visit_EConst _env k =
      EConst (2 * k)
  end in
  v # visit_'expr ()
