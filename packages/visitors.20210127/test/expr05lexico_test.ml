open Expr02
open Expr05lexico

let () =
  assert (compare (EConst 1) (EConst 2) = -1);
  assert (compare (EConst 1) (EAdd (EConst 0, EConst 0)) = -1);
  assert (compare (EAdd (EConst 0, EConst 0)) (EConst 1) = +1);
  assert (compare (EAdd (EConst 0, EConst 0)) (EAdd (EConst 0, EConst 0)) = 0);
  assert (compare (EAdd (EConst 0, EConst 0)) (EAdd (EConst 0, EConst 1)) = -1);
  assert (compare (EAdd (EConst 1, EConst 0)) (EAdd (EConst 0, EConst 1)) = +1);
  ()
