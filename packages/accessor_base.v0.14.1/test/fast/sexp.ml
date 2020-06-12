open! Core_kernel
open! Import

let%expect_test "atoms" =
  let sexp : Sexp.t = List [ Atom "foo"; List [ Atom "bar"; Atom "baz" ] ] in
  Accessor.iter Accessor.Sexp.atoms sexp ~f:print_endline;
  [%expect {|
    foo
    bar
    baz |}];
  sexp |> Accessor.map Accessor.Sexp.atoms ~f:(fun atom -> atom ^ "!") |> print_s;
  [%expect {| (foo! (bar! baz!)) |}]
;;
