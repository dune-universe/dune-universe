open Remu_ts

let _ =
  let open Ty in
  let module M = Ty.PrintAlgebra in
  let open M in
  let t1 = var 1 in
  let t2 = nom 0 in
  let t3 = nom 1 in
  let {str; _} =
    app(t3, tuple [t1; t2])
  in
  print_string str; print_newline();
  let {str; _} = mk_record (module M) ["f1", arrow(t1, t2); "f2", t3]
  in print_endline str
