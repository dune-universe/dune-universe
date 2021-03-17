open Expr15

let size : expr -> int =
  let v = object
    inherit [_] reduce as super
    inherit [_] VisitorsRuntime.addition_monoid
    method! visit_expr env e =
      1 + super # visit_expr env e
  end in
  v # visit_expr ()
