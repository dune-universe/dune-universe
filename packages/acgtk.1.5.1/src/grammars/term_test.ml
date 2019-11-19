open Lexing


let message =
  fun s ->
  match s with
  | 10 ->
     "A full term (constant or term within parenthesis) is expected.\n"
  | 13 ->
     "This opening bracket doesn't have a matching closing bracket.\n"
  | 2 ->
     "Parenthesis should enclose correct lambda-terms.\n"
  | 4 | 3 ->
     "After a binder (e.g., \"lambda\"), variable names are expected.\n"
  | 7 | 0 | 15 ->
     "This closing parenthesis doesn't have a matching opening parenthesis\n"
  | _ ->
     raise Not_found
       
(* A short name for the incremental parser API. *)
       
module I = Grammars.Term_parser_test.MenhirInterpreter
    
(* -------------------------------------------------------------------------- *)

(* The above loop is shown for explanatory purposes, but can in fact be
   replaced with the following code, which exploits the functions
   [lexer_lexbuf_to_supplier] and [loop_handle] offered by Menhir. *)

let succeed (v : Grammars.Term_sequence_parser.term) =
  (* The parser has succeeded and produced a semantic value. Print it. *)
  Printf.printf "Success: %s\n%!" (Grammars.Term_sequence_parser.to_string v)

let fail lexbuf (c : Grammars.Term_sequence_parser.term I.checkpoint) =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  match c with
  | I.HandlingError env ->
     let current_state_num = I.current_state_number env in
     let offset = lexeme_start lexbuf in
     let offset_str = String.make offset ' ' in
     let () = Printf.fprintf stderr
                             "%s^\nAt offset %d: syntax error.\n" offset_str offset in
     Printf.fprintf stderr "%s\n%!" (message current_state_num)
  | _ -> failwith "Should not happen. Alway fails with a HandlingError"

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Grammars.Term_lexer.token lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

(* -------------------------------------------------------------------------- *)

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let lexbuf = from_string line in
  try
    loop lexbuf (Grammars.Term_parser_test.Incremental.main lexbuf.lex_curr_p)
  with
  | Grammars.Term_lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the [calc] demo. *)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Grammars.Term_lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (from_channel stdin)

