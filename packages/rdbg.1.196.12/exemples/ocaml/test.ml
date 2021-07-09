(* Time-stamp: <modified the 29/06/2021 (at 14:58) by Erwan Jahier> *)

(* open Rdbg;; *)
open RdbgEvent;;
open RdbgMain;;
open RdbgStdLib;;

open RdbgArg;;
args.suts <- [Ocaml(Sut_in_ocaml.plugin)];;
args.envs <- [Ocaml(Env_in_ocaml.plugin)];;

args.step_nb <- 100;;

let _ = 
  try
    let e = RdbgMain.run() in
    print_string "RdbgRun.start(): ok.\n";
    flush stdout;
    let e = nexti e 50 in
    print_string "The end.\n";
    flush stdout;
    e.terminate();
    exit 0
  with RdbgEvent.End _ -> 
    print_string "The end for the lurette mode.\n";
    flush stdout;
    
    exit 0


