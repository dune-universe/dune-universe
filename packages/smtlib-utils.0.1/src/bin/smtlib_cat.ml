(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 Simple parser/printer} *)

module A = Smtlib_utils.V_2_6.Ast

let quiet = ref false

type input =
  | Stdin
  | File of string

let string_of_input = function
  | Stdin -> "<stdin>"
  | File f -> Printf.sprintf "file `%s`" f

let process i =
  let l = match i with
    | Stdin -> Smtlib_utils.V_2_6.parse_chan_exn stdin
    | File file -> Smtlib_utils.V_2_6.parse_file_exn file
  in
  if not !quiet then (
    Format.printf "; from %s@." (string_of_input i);
    Format.printf "@[<hv>%a@]@." (A.pp_list A.pp_stmt) l
  )

let process_file f = process (File f)

let options =
  Arg.align [
    "--quiet", Arg.Set quiet, " quiet mode (check only)";
    "-q", Arg.Set quiet, " short for --quiet";
  ]

let () =
  let l = ref [] in
  Arg.parse options (fun s -> l := s :: !l) "usage: tip-cat [file]*";
  try
    if !l=[]
    then process Stdin
    else List.iter process_file (List.rev !l)
  with e ->
    Printexc.print_backtrace stdout;
    print_endline (Printexc.to_string e);
    exit 1
