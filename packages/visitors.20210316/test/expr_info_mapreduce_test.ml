open Expr_info_mapreduce
open Expr_info_mapreduce_use

let mk node = { info = (); node }

let const i =
  mk (EConst i)

let add e1 e2 =
  mk (EAdd (e1, e2))

let e =
  add (const 1) (add (const 0) (const 3))

let (e : int expr) =
  annotate e

let () =
  assert (e.info = 5);
  begin match e.node with
  | EAdd (e1, e2) ->
     assert (e1.info = 1);
     assert (e2.info = 3);
     ()
  | EConst _ ->
     assert false
  end;
  Printf.printf "OK\n%!";
  ()
