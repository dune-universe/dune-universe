(* This script renames some C/FORTRAN functions that are used in
   several codes (especially on netlib) so one can use this library
   together with others. *)

open Printf

let prefix = "odepack_"


let prefix_re = Str.regexp_string_case_fold prefix

let fun_re = Str.regexp_case_fold
               "[^cC].*\\(function\\|subroutine\\) +\\([a-zA-Z0-9_]+\\) *("

let add_names acc file =
  try
    let acc = ref acc in
    let fh = open_in file in
    try
      while true do
        let l = input_line fh in
        if Str.string_match fun_re l 0 then (
          let name = Str.matched_group 2 l in
          if not(Str.string_match prefix_re name 0) then
            acc := name :: !acc
        )
      done;
      assert false
    with End_of_file ->
      close_in fh;
      !acc
  with _  -> acc

let collect files =
  List.fold_left add_names [] files

let substitute1 sub file =
  if Sys.file_exists file then (
    printf "Modifying file %S%!" file;
    let bak = file ^ ".bak" in
    let fh = open_in file in
    let fh1 = open_out bak in
    try
      while true do
        let l = input_line fh in
        if l = "" || l.[0] <> ' ' (* F77 comment *) then (
          output_string fh1 l;
          output_char fh1 '\n'
        )
        else (
          let l =
            List.fold_left (fun l (re, s) -> Str.global_replace re s l) l sub in
          (* Fortran 77 lines must not be more than 72 chars long.
             Otherwise insert a continuation after the last space. *)
          if String.length l > 72 then (
            let i = String.rindex_from l 71 ' ' in
            output_string fh1 (String.sub l 0 i);
            output_string fh1 "\n     & ";
            output_string fh1 (String.sub l i (String.length l - i))
          )
          else
            output_string fh1 l;
          output_char fh1 '\n'
        )
      done;
    with End_of_file ->
      close_in fh;
      close_out fh1;
      Sys.remove file;
      Sys.rename bak file;
      printf "\n%!";
  )

let substitute sub files = List.iter (substitute1 sub) files

let () =
  let protect = ["opkda1.f"; "opkda2.f"] in
  let protect = List.map (Filename.concat "src/fortran/") protect in
  let code = "src/fortran/opkdmain.f" in
  let fn = List.sort compare (collect protect) in
  printf "Functions to rename: %s\n" (String.concat " " fn);
  let sub = List.map (fun n -> (Str.regexp_string n, prefix ^ n)) fn in
  substitute sub (code :: protect)
