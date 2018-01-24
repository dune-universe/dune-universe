(* To see the warnings are with proper locations 

   This module always fail because of the warnings therefore not executed automatically by [omake test]. Test manually by:

   $ ocamlfind ocamlc -g -w A -warn-error +1..50-27 -g -annot -bin-annot -ppx ../ppx/ppx_monadic.opt -I . -c test_warning.ml

*)

open Test_do_.Option

let () =
  return (); (* ok *)
  (do_; return ()); (* ok *)
  (match%m return () with _ -> return ()); (* bad. File"_none_", line1 *)
  ()
