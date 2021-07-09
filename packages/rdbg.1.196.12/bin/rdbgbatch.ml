(* Time-stamp: <modified the 04/07/2021 (at 09:15) by Erwan Jahier> *)
(* Execute (in batch) the instructions contained in luretteSession.cmxs and
   rdbgSession.cmxs files *)

 (* open Rdbg *)
open RifIO
open RdbgArg

(*
open RdbgArg
open Event
open RdbgMain
open GnuplotRif
open Coverage
open RdbgStdLib
open LutinRdbg (* necessary ? *)
*)


(* let main_read_arg () = *)
(*   let tmp_dir =  *)
(*     match args.tmp_dir_provided with  *)
(* 	   | None -> Util.get_fresh_dir Sys.os_type  *)
(*       | Some file -> file   *)
(*   in  *)
(*   args.tmp_dir <- tmp_dir;  *)
(*   Unix.putenv "TMPDIR" (String.escaped tmp_dir)  *)
  	      
let _ =
  ( try RdbgArg.parse Sys.argv;
	 with
	     Failure(e) ->
	       output_string args.ocr e;
	       flush args.ocr ;
	       flush args.ecr ;
	       exit 2
	   | e ->
	     output_string args.ocr (Printexc.to_string e);
	     flush args.ocr;
	     exit 2
  );
 

try 
  List.iter 
    (fun f -> 
       Printf.printf "rdbg-batch dynlinks %s\n" f; flush stdout;
       (* Fails in ocaml 4.08 ! 
          Only impact 'rdbg -lurette', which is equivalent to lurette. 
       *)
       Dynlink.loadfile f
         
       (* Fl_dynload.load_packages fails too, and requires to build an ocamlfind
       package, which is cumbersome *)
       (* Findlib.init (); Fl_dynload.load_packages ~debug:true [f]; *)
    )
    args._others;
  if 
    (args._others = [] &&
        (Printf.eprintf "No cmxs file has been provided to %s.\n%s.\n" 
           Sys.argv.(0) "Starting in Lurette mode"; flush stderr;
         true)
      || 
        not args.rdbg 
    )
  then
    RdbgRun.lurette_start() 
with 
  | Dynlink.Error msg -> 
     Printf.eprintf "\n*** error in rdbg (Dynlink.loadfile %s).\n*** %s.\n%!"
       (List.fold_left (fun acc x -> acc^" "^x) "" args._others)
       (Dynlink.error_message msg);
     RdbgRun.clean_terminate 2
  | End_of_file
  | RdbgEvent.End(_) ->
     RdbgRun.clean_terminate 0
  | pb ->
     let err = Printexc.to_string pb in
     output_string args.ocr err;
     Printf.printf "\nrdbgbatch received '%s'\nbye\n%!" err;
     RdbgRun.clean_terminate 2
;;

let _ =
  Printf.printf "rdbgbatch: bye\n"; flush stdout; exit 0



