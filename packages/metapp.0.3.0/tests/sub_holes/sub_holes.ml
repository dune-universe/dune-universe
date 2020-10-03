[%%metapackage stdcompat]
[%%metaflag "-open", "Stdcompat"]

let () =
  let counter = ref 0 in
  [%meta
     let e = [%e incr counter ] in
     Ppxlib.Ast_helper.Exp.sequence e e];
  Test_framework.assert_eq Int.equal Format.pp_print_int !counter 2

let () =
  match (0, 1, "hello", 3) with
  | [%meta
      Ppxlib.Ast_helper.Pat.tuple (List.init 4 (function
      | 2 -> [%p? x]
      | _ -> [%p? _]))] ->
      Test_framework.assert_eq String.equal Format.pp_print_string x "hello"

let counter = ref 0

[%%meta
  let s = [%stri let () = incr counter ] in
  Metapp_preutils.Stri.of_list [s; s]]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int !counter 2
