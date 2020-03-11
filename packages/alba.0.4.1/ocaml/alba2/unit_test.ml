open Lib
open Common_module_types



(* Library modules *)
let _ =
  Document.test ();
  Pretty_printer.test ();
  Pretty_printer2.test ();
  Character_parser.test ()



(* Albatross modules *)
let _ =
  Type_checker.test()
