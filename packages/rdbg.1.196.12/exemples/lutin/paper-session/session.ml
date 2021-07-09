open Event;;
open RdbgMain;;
open RdbgStdLib;;

(* on verra pour le sucre plus tard... *)
open RdbgArg;;
open LutinRun;;


args.suts <- [Ocaml(Dynlink.adapt_filename "sut.cma")];;
args.envs <- [Ocaml(Dynlink.adapt_filename "env.cma")];;

args.step_nb <- 500;
args.seed <- Some 1;;  

RdbgStdLib.show_src := true

open RdbgRun

let main () = 
  try 
    Printf.printf "Starting...\n"; flush stdout;
    let e = run() in
    Printf.printf "Step 5:\n"; flush stdout;
     let e =  stepi e 5 in
     e.terminate()
  with 
    | Event.End(_) ->  
      Printf.printf  "Profiler result:%s\n" (LutinRdbg.dump_profile_info());
      Printf.printf "bye\n"; flush stdout
    | err -> 
      Printf.printf "pb in test.ml :  %s\n"  (Printexc.to_string err);
      flush stdout;
      exit 2
      

(*  let _ = main (); clean_terminate(); exit 0 *)
