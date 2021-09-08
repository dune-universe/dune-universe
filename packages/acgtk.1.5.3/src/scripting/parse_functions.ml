open AcgData.Environment
open Functions

module I = Command_parser.MenhirInterpreter
module Error = AcgData.Error
             
(* -------------------------------------------------------------------------- *)
             
(* The above loop is shown for explanatory purposes, but can in fact be
   replaced with the following code, which exploits the functions
   [lexer_lexbuf_to_supplier] and [loop_handle] offered by Menhir. *)
             
let succeed (data : (Functions.context * Environment.t -> Functions.context * Environment.t)) =
  (* The parser has succeeded and produced a semantic value. *)
  data
    
let fail lexbuf (c : (Functions.context * Environment.t -> Functions.context * Environment.t) I.checkpoint) =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  match c with
  | I.HandlingError env ->
     let loc = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf in
     let current_state_num = I.current_state_number env in
     raise Error.(Error (Parse_error (Syntax_error ((Messages.message current_state_num)),loc)))
  | _ -> failwith "Should not happen. Always fails with a HandlingError"
  | exception Not_found ->
     let loc = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf in
     raise Error.(Error (Parse_error (Syntax_error (""),loc)))

           
let core_supplier lexbuf = I.lexer_lexbuf_to_supplier Script_lexer.lexer lexbuf

let supplier = core_supplier

let parse_file filename ctx env =
  try
    let in_ch =
      let fullname = UtilsLib.Utils.find_file filename (Functions.dirs ctx)  in
      open_in fullname in
    let lexbuf = Lexing.from_channel in_ch in
    let () = Logs.app (fun m -> m  "Parsing script file \"%s\"..." filename) in
    let l_ctx = Functions.set_filename (Some filename) ctx in
    let l_ctx',new_env=
      try
        (I.loop_handle
           succeed
           (fail lexbuf)
           (supplier lexbuf)
           (Command_parser.Incremental.commands lexbuf.Lexing.lex_curr_p))
          (l_ctx,env)
      with
      | Failure f when f="lexing: empty token" ->
         raise AcgData.Error.(Error (Lexer_error (Expect "Bad end of input. ';'",(Lexing.lexeme_end_p lexbuf,Lexing.lexeme_end_p lexbuf)))) in
    let ctx' = Functions.set_filename (Functions.get_filename ctx) l_ctx' in
    let () = Logs.app (fun m -> m "Done.") in
    ctx',new_env
  with
  | UtilsLib.Utils.No_file(f,msg) ->
     let e = AcgData.Error.System_error (Printf.sprintf "No such file \"%s\" in %s" f msg) in
     let () = Logs.err (fun m -> m "%s" (AcgData.Error.error_msg e filename)) in
     let _ = Script_lexer.reset_echo () in
     ctx,env
  | Sys_error s ->
     let e = AcgData.Error.System_error s in
     let () = Logs.err (fun m -> m "%s" (AcgData.Error.error_msg e filename)) in
     let _ = Script_lexer.reset_echo () in
     ctx,env
  | AcgData.Error.Error e -> 
     let () = Logs.err (fun m -> m "%s" (AcgData.Error.error_msg e filename)) in
     let _ = Script_lexer.reset_echo () in
     ctx,env
  | Scripting_errors.Error (e,p,f) ->
     let () = Logs.err (fun m -> m "%s" (Scripting_errors.error_msg e p f)) in
     let _ = Script_lexer.reset_echo () in
     ctx,env
       
       
let commented_regexp = Str.regexp "^[ \t#]*#"
                                  
let is_fully_commented_line s = Str.string_match commented_regexp s 0
                                                 
let read_line_from_in_ch in_ch =
  let () = flush stdout in
  input_line in_ch
             
let bufferize in_ch =
  let () = Printf.printf "# " in
  let buf = Buffer.create 16 in
  let no_semi_colon=ref true in
  let () =
    while !no_semi_colon do
      let input = read_line_from_in_ch in_ch in
      if not (is_fully_commented_line input) then
	try
	  let semi_colon_index=String.index input ';' in
	  let () = Buffer.add_string buf (String.sub input 0 (semi_colon_index+1)) in
	  no_semi_colon:=false
	with
	| Not_found ->
	   Buffer.add_string buf input ;
	   Buffer.add_char buf '\n';
	   Printf.printf "  "
      else
	()
    done in
  Buffer.contents buf

type 'a result =
  | Stop of 'a
  | Continue of 'a
              

                  
let parse_entry ~resize in_ch ctx env =
  let in_str = bufferize in_ch in
  let lexbuf = Lexing.from_string in_str in
  let () = 
    if resize then
      let () = UtilsLib.Utils.sterm_set_size () in
      UtilsLib.Utils.term_set_size ()
    else
      () in
  try
    Continue ((I.loop_handle
                  succeed
                  (fail lexbuf)
                  (supplier lexbuf)
                  (Command_parser.Incremental.commands lexbuf.Lexing.lex_curr_p))
                 (ctx,env))
  with
  | Functions.Quit -> Stop (ctx,env)
  | Functions.Stop -> Stop (ctx,env)
  | Failure f when f="lexing: empty token" -> Continue (ctx,env)
  | AcgData.Error.Error e -> 
     let () = Logs.err (fun m -> m "%s" (AcgData.Error.error_msg e "stdin")) in
     let _ = Script_lexer.reset_echo () in
     Continue (ctx,env)
  | Scripting_errors.Error (e,p,f) ->
     let () = Logs.err (fun m -> m "%s" (Scripting_errors.error_msg e p f)) in
     let _ = Script_lexer.reset_echo () in
     Continue (ctx,env)

