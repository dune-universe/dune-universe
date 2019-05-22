open Batteries;;
open Jhupllib;;
open OUnit2

let all_tests =
  [ Test_reachability.tests;
    Test_reachability_primes.tests
  ];;

let () =
  begin
    try
      let logging_instructions = Sys.getenv "LOG" in
      let parse_module_level module_level_str =
        match Logger_utils.level_of_string module_level_str with
        | Some x -> x
        | None -> failwith @@ "Invalid logging level: " ^ module_level_str
      in
      if BatString.exists logging_instructions "=" then
        let (module_name,module_level_str) =
          String.split logging_instructions ~by:"="
        in
        Logger_utils.set_logging_level_for module_name @@
          parse_module_level module_level_str
      else
        Logger_utils.set_default_logging_level @@
          parse_module_level logging_instructions
    with
    | Not_found -> ()
  end;
  run_test_tt_main ("Tests" >::: all_tests)
;;
