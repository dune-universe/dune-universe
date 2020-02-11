open Expr00

let count (e : expr) : int =
  let v = object
    val mutable count = 0
    method count = count
    inherit [_] iter as super
    method! visit_EAdd env e0 e1 =
      count <- count + 1;
      super#visit_EAdd env e0 e1
  end in
  v#visit_expr () e;
  v#count
