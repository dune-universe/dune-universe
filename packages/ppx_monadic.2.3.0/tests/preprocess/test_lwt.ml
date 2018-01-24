(* to test: ocamlfind ocamlc -dsource -package ppx_monadic,lwt.ppx,lwt -c test_lwt.ml *)
(* ppx_monadic once removed let%xxx where xxx <> m by mistake *)

let _ = let%lwt a = Lwt.return 5 in print_int a; Lwt.return_unit
