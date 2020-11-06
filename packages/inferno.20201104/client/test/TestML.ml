open Client

(* A few manually constructed terms. *)

let x =
  ML.Var "x"

let y =
  ML.Var "y"

let id =
  ML.Abs ("x", x)

let delta =
  ML.Abs ("x", ML.App (x, x))

let deltadelta =
  ML.App (delta, delta)

let idid =
  ML.App (id, id)

let k =
  ML.Abs ("x", ML.Abs ("y", x))

let genid =
  ML.Let ("x", id, x)

let genidid =
  ML.Let ("x", id, ML.App (x, x))

let genkidid =
  ML.Let ("x", ML.App (k, id), ML.App (x, id))

let genkidid2 =
  ML.Let ("x", ML.App (ML.App (k, id), id), x)

let app_pair = (* ill-typed *)
  ML.App (ML.Tuple [id; id], id)

let () =
  assert Test.(Log.with_log CheckML.test idid);
  assert Test.(Log.with_log CheckML.test genid);
  assert Test.(Log.with_log CheckML.test genidid);
  assert Test.(Log.with_log CheckML.test genkidid);
  assert Test.(Log.with_log CheckML.test genkidid2);
  (* we include some printing below because Dune
     appears to only show tests that show some output. *)
  print_endline "TestML: all tests passed."
