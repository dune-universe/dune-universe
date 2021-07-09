
open RdbgEvent;;
open RdbgMain;;
open RdbgStdLib;;

(* such kind of stuff can now be generated via rdbg  *)
open RdbgArg;;
open Lv6Run;;

let sut_plugin = 
  let args = ["lv6";"heater_control.lus";"-n";"heater_control"] in
  let aargs = Array.of_list args in
  Printf.printf "Lv6Run.make %s\n" (String.concat " " args); flush stdout;
  Lv6Run.make aargs

let env_plugin = 
  let args = ["lv6";"heater_env.lus";"-n";"heater_env"] in
  let aargs = Array.of_list args in
  Printf.printf "Lv6Run.make %s\n" (String.concat " " args); flush stdout;
  Lv6Run.make aargs
;;

args.suts <- [Ocaml(sut_plugin)];;
args.envs <- [Ocaml(env_plugin)];;


args.step_nb <- 5;;
(* args.verbose <- 50;; *)
args.debug_rdbg <- true;;
(* Dynlink.allow_unsafe_modules true;; *)



open RdbgRun ;;
let main () =
  try 
    let e = RdbgRun.start()  in
    let _e =  stepi e args.step_nb in
    ()
  with 
    | RdbgEvent.End(_) -> ()
    |  e -> 
      Printf.printf "pb in test.ml:  %s\n"  (Printexc.to_string e);
      exit 2
;;


let _ = try
  main (); 
  Printf.eprintf "main(): ok\n";flush stderr;
  clean_terminate 0
  
  with
  | RdbgEvent.End _i -> ()
  | e ->
     (Printf.printf "pb in test.ml :  %s\n"  (Printexc.to_string e) );
     assert false

