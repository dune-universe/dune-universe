exception CantOverwrite of string

let invalid_commit cfile =
  "Failed to parse commitment box from file "
  ^ cfile
  ^ "! It's possibly an invalid file..."


let invalid_open ofile =
  "Failed to parse opening key from file "
  ^ ofile
  ^ "! It's possibly an invalid file..."


let invalid_pairs cfile ofile =
  "Failed to reveal the secret from both commitment file "
  ^ cfile
  ^ " and opening file"
  ^ ofile
  ^ "! They are possibly made from different commitment generation calls..."


let system_error msg = "Fatal system error, " ^ msg

let cant_overwrite file =
  raise
    (CantOverwrite
       ( "Can't overwrite the file "
       ^ file
       ^ "! Use the flag --force here if you really need to do that..." ))


let write_to data file =
  let handler = open_out file in
  output_string handler data ;
  flush handler ;
  close_out handler


let read_line handler =
  try Some (input_line handler) with End_of_file -> None


let rec read_loop buffer handler =
  match read_line handler with
  | None ->
      buffer
  | Some line ->
      read_loop (buffer ^ "\n" ^ line) handler


let read_some handler =
  match read_line handler with None -> "" | Some line -> line


let read_all handler =
  let first = read_some handler in
  read_loop first handler


let read_from file =
  let handler = open_in file in
  let lines = read_all handler in
  close_in handler ;
  lines
