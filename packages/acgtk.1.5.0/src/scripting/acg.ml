(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open UtilsLib
open Scripting
open Functions

let () = Sys.catch_break true


let dirs = ref [""]

let pp_output = ref true
let svg_output = ref (Some "realize.svg")

let dont_exit_on_end_of_file = ref false

module P = Parse_functions

module F = Functions

let options =
  [
    ("-version", Arg.Unit (fun () -> Printf.printf "%s\n" Version.version;exit 0), " Prints the version number");
    ("-I", Arg.String (fun dir -> dirs := (!dirs)@[dir]) , " -I dir sets dir as a directory in which file arguments can be looked for");
    ("-nc"), Arg.Unit (fun () -> F.color_output false), " toggle off the colors on the output";
    ("-npp"), Arg.Unit (fun () -> let () = Utils.no_pp () in pp_output:=false), " toggle off the pretty printing of the output";
    ("-svg"), Arg.String (fun filename -> svg_output:=Some filename), " set the file name of the svg output of the \"realize\" acg command";
    ("-nsvg"), Arg.Unit (fun () -> svg_output:=None), " toggle off the svg output";
    ("-realize", Arg.String (fun file -> F.set_config file !dirs), " sets the config rendering for the realize.svg files");
  (*    ("-b"), Arg.Unit (fun () -> dont_exit_on_end_of_file:=true), " does not exit when reaching the end of the input file"; *)
  ]

let usg_msg = Printf.sprintf "%s [options] file1 file2 ...\n\nThis will parse the files which are supposed to be files acripting commands and then run the ACG command interpreter." (Filename.basename Sys.executable_name)


let welcome_string = "Welcome to the ACG toplevel"
let version_string = Printf.sprintf "Version %s" Version.version
let copyright_string = "©INRIA 2008-2018"
let bug_string = "Please send your comments or bug reports or feature requests to sylvain.pogodalla@inria.fr"


let env = ref AcgData.Environment.Environment.empty

let resize_terminal () =
  if !pp_output then
    let () = Utils.sterm_set_size () in
    Utils.term_set_size ()
  else
    ()

let welcome_msg () =
  let () = resize_terminal () in
  let l = 
    let l = Format.get_margin () in
    if l > 1000 then
      ((String.length bug_string)+10)
    else
      l in
  let _ = Format.flush_str_formatter () in
  let () = Format.printf "@[<v>" in
  let () = 
    List.iter
      (fun s -> 
	let prepend =
	  try
	    (String.make ((l-(String.length s))/2) ' ')
	  with
	  | Invalid_argument _ -> "" in
	Format.printf "@[<h>%s%s@]@," prepend s)
      [welcome_string;version_string;copyright_string;bug_string] in
  Format.printf "@,@[<v>@[<v15>Type@,help;@]@,@[to@ get@ help.@]@]@,@]@.@?"
    
    
let printed_welcome_message = ref false

let print_welcome_message () = 
  if !printed_welcome_message then
    ()
  else
    let () = welcome_msg ()in
    printed_welcome_message := true


let anon_fun s =
  let () = print_welcome_message () in
  let () = resize_terminal () in
  let ctx = F.context ~wait:false ~echo:true ~svg:!svg_output ~dirs:!dirs ~parse_fun:P.parse_file in
  env := snd (P.parse_file s ctx !env)


let invite () = 
  let () = Arg.parse options anon_fun usg_msg in
  print_welcome_message ()
    
let main first =
  (* ANSITerminal get the size info from stdin In case of redirection,
     the latter may not be set. That's why it is first duplicated and
     stdin is then duplicated from stdout *)
  let stdin_tmp=Unix.dup Unix.stdin in
  let stdin_tmp_in_ch = Unix.in_channel_of_descr stdin_tmp in
  let () = Unix.dup2 Unix.stdout Unix.stdin in
  let () = if first then invite () else () in
  let continue = ref true in
  let () = resize_terminal () in
  while !continue do
    try
      let ctx = F.context ~wait:false ~echo:true ~svg:!svg_output ~dirs:!dirs ~parse_fun:P.parse_file in
      let () = env := snd (P.parse_entry ~resize:!pp_output stdin_tmp_in_ch ctx !env) in
      Format.print_flush ()
    with
    | End_of_file ->
       if !dont_exit_on_end_of_file then
	 ()
       else
	 continue:=false
  done


let wait = Unix.sleep

    
let _ =
  let () = Log.set_level ~app:"acg" Logs.Warning in
  (*
  let c = Mtime_clock.counter () in
  Logs.info (fun m -> m "Starting run");
  let delay1, delay2, delay3 = 1, 2, 4 in
  Logs.info (fun m -> m "Start action 1 (%d)." delay1 ~tags:(Log.stamp c));
  wait delay1;
  Logs.info (fun m -> m "Start action 2 (%d)." delay2 ~tags:(Log.stamp c));
  wait delay2;
  Logs.info (fun m -> m "Done." ?header:None ~tags:(Log.stamp c));
   *)
  let () = 
    try main true with
    | F.Stop -> ()
    | F.Quit -> ()
    | Sys.Break -> () in
  Logs.app (fun m -> m "Goodbye.")
    
