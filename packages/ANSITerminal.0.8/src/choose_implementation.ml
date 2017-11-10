(* Script to choose the unix or windows implementation depending on
   the platform *)

open Printf

let copy_file ?(line_directive=false) ?(dir=".") source target =
  let fh0 = open_in (Filename.concat dir source) in
  let target = Filename.concat dir target in
  (try Sys.remove target with _ -> ());
  let fh1 = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o444 target in
  let content = Buffer.create 4096 in
  if line_directive then
    bprintf content "#1 \"%s\"\n" (Filename.concat dir source);
  Buffer.add_channel content fh0 (in_channel_length fh0);
  Buffer.output_buffer fh1 content;
  close_in fh0;
  close_out fh1


let choose_unix () =
  copy_file "ANSITerminal_unix.ml" "ANSITerminal.ml" ~line_directive:true;
  copy_file "ANSITerminal_unix_stubs.c" "ANSITerminal_stubs.c"

let choose_win () =
  copy_file "ANSITerminal_win.ml" "ANSITerminal.ml" ~line_directive:true;
  copy_file "ANSITerminal_win_stubs.c" "ANSITerminal_stubs.c"

let () =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> choose_unix ()
  | "Win32" -> choose_win()
  | e -> eprintf "Unknown OS type %S.\n" e
