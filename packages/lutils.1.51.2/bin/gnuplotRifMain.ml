(* Time-stamp: <modified the 01/02/2019 (at 09:04) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: gnuplotrif.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
*)

open GnuplotRif

let usage = (Sys.argv.(0) ^" [options] <f>.rif

Generates a <f>.gp file such that gnuplot can plot the rif file.
"^ressource_file_usage^
"
Command-line options are handled afterwards.
")

(* Cloned from the OCaml stdlib Arg module: I want it on stdout! (scrogneugneu) *)
let usage_out speclist errmsg =
  Printf.printf "%s" (Arg.usage_string speclist errmsg)

let rec speclist =
  [
    "-no-display",Arg.Unit (fun _ -> (terminal := NoDisplay)),
    "\t generate the .gp file, without launching gnuplot";

    "-dyn",Arg.Unit (fun _ -> (dynamic := true)),
    "\t dynamically display the end of the rif file";

    "-size",Arg.Int (fun i -> (window_size := (i))),
    "\t<s> set the size of the sliding window in -dyn mode";

    "-min",Arg.Int (fun i -> (min_step := Some (i+1))),
    "\t<min> only display steps > min (ignored in -dyn mode)";

    "-max",Arg.Int (fun i -> (max_step := Some (i+1))),
    "\t<max> only display steps < max (ignored in -dyn mode)";

    "-nogrid",Arg.Unit (fun _ -> (grid := false)),
    "remove the grid (useful with -dyn)\n";

    "--hide-var",Arg.String (fun str -> (vars_to_hide := str :: !vars_to_hide)),
    "<string> hide a variable (one can use the wildcard '*')";

    "-hv",Arg.String (fun str -> (vars_to_hide := str :: !vars_to_hide)),
    "<string> shortcut for --hide-var\n";

    "--show-var",Arg.String (fun str -> (vars_to_show := str :: !vars_to_show)),
    "<string> show a wildcard-hided variable";
    
    "-sv",Arg.String (fun str -> (vars_to_show := str :: !vars_to_show)),
    "<string> shortcut for --show-var

Changing the default gnuplot terminal:";

    "-wxt",Arg.Unit (fun _ -> (terminal := Wxt)),
    "\t launch gnuplot with the wxt terminal";

    "-qt",Arg.Unit (fun _ -> (terminal := Qt)),
    "\t launch gnuplot with the qt terminal";

    "-x11",Arg.Unit (fun _ -> (terminal := X11)),
    "\t launch gnuplot with the X11 terminal";

    "-jpg",Arg.Unit (fun _ -> (terminal := Jpg)),
    "\t output in a jpg file";

    "-pdf",Arg.Unit (fun _ -> (terminal := Pdf)),
    "\t output in a pdf file";

    "-ps",Arg.Unit (fun _ -> (terminal := Ps)),
    "\t output in a B&W post-script file";

    "-cps",Arg.Unit (fun _ -> (terminal := Cps)),
    "\t output in a color post-script file";

    "-eps",Arg.Unit (fun _ -> (terminal := Eps)),
    "\t output in a color encapsulated post-script file";

    "-latex",Arg.Unit (fun _ -> (terminal := Latex)),
    " output in a latex file\n";

    "--verbose",Arg.Unit (fun _ -> (verbose := true)),
    "";
    "-verbose",Arg.Unit (fun _ -> (verbose := true)),
    "";
    "-v",Arg.Unit (fun _ -> (verbose := true)),
    "\t\t set on a verbose mode";

    "--help", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    "";
    "-help", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    "";
    "-h", Arg.Unit (fun _ -> (usage_out speclist usage ; exit 0)),
    "\t\t display this help message"
  ]


let main () =
  read_ressource_file ();
  if   (Array.length Sys.argv)  <= 1
  then (Arg.usage speclist usage; flush stdout; exit 2)
  else (
    try Arg.parse speclist (fun s -> rif_file := s) usage
    with  
      | Failure(e) -> print_string e; flush stdout; flush stderr; exit 2
	   | e -> print_string (Printexc.to_string e);  flush stdout; exit 2
  );
  let oc,pid =  GnuplotRif.f () in
  let rec rloop () =
    let str = read_line () in
    if str = "q" || str = "quit" then (
      Unix.kill pid Sys.sigkill; 
      close_out oc;
      exit 0
    ) else (
      output_string oc (str^"\n"); flush oc;
      rloop()
    )
  in
  match !terminal with
  | X11| Wxt | Qt | Default ->  rloop ()
  | Jpg | Ps | Pdf | Cps | Eps | Latex | NoDisplay ->  ()

let _ = main ()
