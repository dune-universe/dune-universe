open RdbgEvent;;
open RdbgMain;;
open RdbgStdLib;;
open RdbgArg;;
open Lv6Run;;


let s2str (n,v) = (n^"="^(Data.val_to_string string_of_float v))
let print_local e =
  if e.kind=Exit then (
    print_string "# locals: ";
    List.iter 
      (fun (n,v) -> if n.[0] <> '_' then print_string (s2str (n,v) ^ " ") else ())
      e.data;
    print_string "\n"; flush stdout
  )
      
(* Will call print_local at each event *)
let _ = add_hook "print_event" print_local

let _ = 
  let call = read_line () in (* lv6 should be called here (cf Makefile) *)
  let zeargs = Str.split (Str.regexp " ") call in
  let sut_plugin = Lv6Run.make (Array.of_list zeargs) in
  Printf.eprintf "Lv6Run.make %s\n" (String.concat " " zeargs); flush stderr;
  args.suts <- [Ocaml(sut_plugin)];
(*   show_trace := false; *)
  add_hook "print_event" (fun e -> print_local e);
  let e = run () in
  let _e = nexti e 10000 in
  exit 0
