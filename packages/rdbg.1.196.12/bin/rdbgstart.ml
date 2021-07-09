(* Time-stamp: <modified the 26/05/2021 (at 11:48) by Erwan Jahier> *)

(* open Rdbg *)
open RdbgArg
open RifIO

(*
open RdbgStdLib (* so that they are included by ocamlbuild *)
open Callgraph (* ditto *)
open Tagcov (* ditto *)
open Event
open RdbgMain
open GnuplotRif
open Coverage
open LutinRdbg
*)
(* open LutinRun *)
(* open Value *)

(* faire comme dans rdbgbatch pour les arguments *)

let myexit i = 
  if args.rdbg then failwith "error in rdbg-top" else exit i

(* Taken from 
http://stackoverflow.com/questions/6158596/setting-the-prompt-in-an-ocaml-custom-toplevel 
*)
let _ = 
  (Toploop.read_interactive_input :=  
     let old = !Toploop.read_interactive_input in 
     fun _prompt buffer len ->
     old (RdbgMain.get_prompt ()) buffer len
  );
  flush stderr;
  flush stdout;
  if RdbgMain.get_prompt () <> "" then (
    output_string stdout ("        Rdbg Version \""^(RdbgVersion.str)^
                          "\" (\""^RdbgVersion.sha^"\") \n");
    flush stdout
  );
  ignore (Topmain.main());
  ()
  
