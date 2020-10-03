open! Core
open! Async
open! Import

let set_of_chars f = Set.of_list (module Char) (List.filter [%all: Char.t] ~f)

(* OCaml manual 7.1 Lexical conventions
   https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#lowercase-ident *)
let valid_variable_name =
  let valid_first_character =
    set_of_chars (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let valid_character =
    set_of_chars (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  fun variable_name ->
    String.length variable_name > 0
    && Set.mem valid_first_character variable_name.[0]
    && String.for_all variable_name ~f:(Set.mem valid_character)
;;

let test ~print filename =
  let variable_name = Embed_file_lib.Private.variable_name_of_file_name filename in
  if valid_variable_name variable_name
  then (if print then print_endline variable_name)
  else print_cr [%here] [%message (filename : string) (variable_name : string)]
;;

let%expect_test "examples" =
  let test = test ~print:true in
  test "2020-04-22-sexp-grammar.org";
  [%expect {| _2020_04_22_sexp_grammar_dot_org |}];
  test "int0.ml";
  [%expect {| int0_dot_ml |}];
  test "i_string_UTF8_surrogate_U+D800.json";
  [%expect {| i_string_utf8_surrogate_u_0x2b_d800_dot_json |}];
  test "n_structure_trailing_#.json";
  [%expect {| n_structure_trailing__0x23__dot_json |}];
  return ()
;;

let%expect_test "exhaustive" =
  let filenames =
    (* every possible 1 and 2 character file name *)
    List.map [%all: Char.t] ~f:Char.to_string
    @ List.map [%all: Char.t * Char.t] ~f:(fun (c1, c2) -> String.of_char_list [ c1; c2 ])
  in
  List.iter filenames ~f:(test ~print:false);
  [%expect {| |}];
  return ()
;;
