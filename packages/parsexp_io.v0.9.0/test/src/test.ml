open Core

let test filename =
  match Parsexp_io.load (module Parsexp.Many) ~filename with
  | Ok _ -> ()
  | Error error ->
    Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename
;;

let%expect_test _ =
  test "jbuild";
  [%expect];
  test "invalid-sexp";
  [%expect{|
    File "invalid-sexp", line 3, character 6:
    Error: s-expression parsing error;
    unterminated decimal escape sequence
  |}];
;;
