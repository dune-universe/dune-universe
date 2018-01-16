(** Small @var@ replacement language *)

(* using good old Str *)
open Str

let replace_variables f =
  let rex = regexp "@[A-Za-z0-9_]+@" in
  let replace = 
    global_substitute rex (fun s ->
      let matched = matched_string s in
      let k = String.sub matched 1 (String.length matched - 2) in
      f k)
  in
  replace

let replace_file f path outpath =
  let ic = open_in path in
  let oc = open_out outpath in
  let rec loop () = 
    try 
      let line = input_line ic in
      output_string oc (replace_variables f line);
      output_char oc '\n';
      loop ()
    with
    | End_of_file -> close_in ic; close_out oc
  in
  loop ()
