open UtilsLib
module I = Dl_parser.MenhirInterpreter


         
(* -------------------------------------------------------------------------- *)

let succeed data =
  (* The parser has succeeded and produced a semantic value. *)
  data
  
let fail lexbuf c =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  match c with
  | I.HandlingError env ->
     let loc = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf in
     let current_state_num = I.current_state_number env in
     raise DlError.(Error.(Error (SyntError (ParseError (Messages.message current_state_num)),loc)))
  | _ -> failwith "Should not happen. Always fails with a HandlingError"

let core_supplier lexbuf = I.lexer_lexbuf_to_supplier Dl_lexer.lexer lexbuf

let supplier = core_supplier

let dummy_loc = Lexing.(dummy_pos,dummy_pos)

type entry =
  | File of string
  | String of string
              
let generic_parse ~entry f =
  let input,input_name =
    match entry with
    | File f -> Some f,f
    | String q -> None,q in
  try
    let lexbuf,from_file =
      match entry with
      | File filename -> 
         let in_ch =
           let fullname = Utils.find_file filename [""]  in
           open_in fullname in
         Lexing.from_channel in_ch,true
      | String s -> Lexing.from_string s,false in
    let () = if from_file then Logs.app (fun m -> m "Parsing \"%s\"..." input_name) else () in
    let starting_parse_time = Sys.time () in
    let e = I.loop_handle succeed (fail lexbuf) (supplier lexbuf) (f lexbuf.Lexing.lex_curr_p) in
    let ending_parse_time = Sys.time () in
    let () = if from_file then Logs.app (fun m -> m "Done (%.3f seconds).\n%!" (ending_parse_time -. starting_parse_time)) else () in
    Some e
  with
  | Utils.No_file(f,msg) ->
     let e =  DlError.Error.SysError (Printf.sprintf "No such file \"%s\" in %s" f msg) in 
     let () = Logs.err (fun m -> m "%s" (DlError.Error.error_msg (e,dummy_loc) ~filename:f)) in
     None
  | Sys_error s ->
     let e = DlError.Error.SysError s in
     let () = Logs.err (fun m -> m "%s" (DlError.Error.error_msg (e,dummy_loc) ?filename:input)) in
     None
  | DlError.Error.Error e ->
     let () =
       match entry with
       | File _ -> ()
       | String s ->
          Logs.err (fun m -> m "Error while parsing \"%s\"." s) in
     let () = Logs.err (fun m -> m "%s" (DlError.Error.error_msg e ?filename:input)) in
     None
     
let parse_program filename =
  generic_parse ~entry:(File filename) Dl_parser.Incremental.program


let parse_edb filename =
  generic_parse ~entry:(File filename) Dl_parser.Incremental.extensional_facts


let parse_query q =
  generic_parse ~entry:(String q) Dl_parser.Incremental.query
  
