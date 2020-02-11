open Expr02

let tag : expr -> int = function
  | EConst _ -> 0
  | EAdd _   -> 1

exception Different of int

let compare (i1 : int) (i2 : int) : unit =
  if i1 <> i2 then
    raise (Different (if i1 < i2 then -1 else 1))

class compare = object
  inherit [_] iter2
  method! visit_int  _ i1 i2 = compare i1 i2
  method! fail_expr () e1 e2 = compare (tag e1) (tag e2)
end

let compare (e1 : expr) (e2 : expr) : int =
  try new compare # visit_expr () e1 e2; 0 with Different c -> c
