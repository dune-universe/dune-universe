
let sut_plugin = 
  let args = ["lutin";"crazy_controleur.lut";"-main";"heater_control";"-seed";"42"] in
  let aargs = Array.of_list args in
  Printf.printf "LutinRun.make %s\n" (String.concat " " args); flush stdout;
  LutinRun.make aargs

let env_plugin = 
  let args = ["lutin";"degradable-sensors.lut";"-main";"main";"-seed";"42"] in
  let aargs = Array.of_list args in
  Printf.printf "LutinRun.make %s\n" (String.concat " " args); flush stdout;
  LutinRun.make aargs
;;

open RdbgArg;;
args.suts <- [Ocaml(sut_plugin)];;
args.envs <- [Ocaml(env_plugin)];;

args.step_nb <- 50;;

LutinRdbg.profiler true;;


open RdbgEvent;;
open RdbgMain;;
open RdbgStdLib;;
open RdbgRun



let main () = 
  try 
(*     show_src := true; *)
    Printf.printf "Starting...\n"; flush stdout;
    let e = run() in
    Printf.printf "Step 50:\n"; flush stdout;
     let e =  stepi e 49  in
     e.terminate()
  with 
    | RdbgEvent.End(_) ->  
      Printf.printf  "Profiler result:%s\n" (LutinRdbg.dump_profile_info());
      Printf.printf "bye\n"; flush stdout
    | err -> 
      Printf.printf "pb in test.ml :  %s\n"  (Printexc.to_string err);
      flush stdout;
      exit 2
      

let _ = main (); clean_terminate 0
open RdbgArg;;
  

