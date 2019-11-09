(* ocamlc -I ../_build/src -custom ../_build/src/mpfr.cmo ../utils/mpfr_version.ml ../_build/src/mlmpfr_stubs.o -ccopt -L/usr/local/lib -cclib -lmpfr *)

module M = Mpfr

let _ =
  Printf.printf "%s\n" (M.get_version ())
