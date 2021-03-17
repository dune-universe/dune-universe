open Expr01

let add e1 e2 =
  match e1, e2 with
  | EConst 0, e
  | e, EConst 0 -> e
  | _, _ ->        EAdd (e1, e2)

let optimize : expr -> expr =
  let o = object (self)
    inherit [_] map
    method! visit_EAdd env e1 e2 =
      add
        (self#visit_expr env e1)
        (self#visit_expr env e2)
  end in
  o # visit_expr ()

let z e = EAdd (e, EConst 0)

let () =
  assert (optimize (z (EConst 1)) = EConst 1);
  assert (optimize (z (z (EConst 1))) = EConst 1);
  assert (optimize (EAdd (EConst 1, EConst 1)) = EAdd (EConst 1, EConst 1));
  assert (optimize (EAdd (z (EConst 1), EConst 1)) = EAdd (EConst 1, EConst 1));
