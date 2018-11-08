open UtilsLib
open AcgData
open Environment
open Logic

       
(* A short name for the incremental parser API. *)

let tok_to_string = function
  | Data_parser.EOI -> "EOI"
  | Data_parser.LPAREN _ -> "LPAREN"
  | Data_parser.RPAREN _ -> "RPAREN"
  | Data_parser.RSQBRACKET _ -> "RSQBRACKET"
  | Data_parser.LSQBRACKET _ -> "LSQBRACKET"
  | Data_parser.SIG_OPEN _ -> "SIG_OPEN"
  | Data_parser.LEX_OPEN _ -> "LEX_OPEN"
  | Data_parser.NL_LEX_OPEN _ -> "NL_LEX_OPEN"
  | Data_parser.END_OF_DEC _ -> "END_OF_DEC"
  | Data_parser.IDENT (s,_) -> Printf.sprintf "IDENT (%s)" s
  | Data_parser.COLON _ -> "COLON"
  | Data_parser.EQUAL _ -> "EQUAL"
  | Data_parser.SEMICOLON _ -> "SEMICOLON"
  | Data_parser.COMPOSE _ -> "COMPOSE"
  | Data_parser.SYMBOL (s,_) -> Printf.sprintf "SYMBOL (%s)" s
  | Data_parser.COMMA _ -> "COMMA"
  | Data_parser.TYPE _ -> "TYPE"
  | Data_parser.PREFIX _ -> "PREFIX"
  | Data_parser.INFIX _ -> "INFIX"
  | Data_parser.BINDER _ -> "BINDER"
  | Data_parser.COLON_EQUAL _ -> "COLON_EQUAL"
  | Data_parser.LAMBDA _ -> "LAMBDA"
  | Data_parser.LAMBDA0 _ -> "LAMBDA0"
  | Data_parser.DOT _ -> "DOT"
  | Data_parser.ARROW _ -> "ARROW"
  | Data_parser.LIN_ARROW _ -> "LIN_ARROW"

       
module I = Data_parser.MenhirInterpreter
    
(* -------------------------------------------------------------------------- *)

(* The above loop is shown for explanatory purposes, but can in fact be
   replaced with the following code, which exploits the functions
   [lexer_lexbuf_to_supplier] and [loop_handle] offered by Menhir. *)

let succeed (data : (?overwrite:bool -> Environment.t -> Environment.t)) =
  (* The parser has succeeded and produced a semantic value. *)
  data

let fail lexbuf c =
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

let core_supplier lexbuf = I.lexer_lexbuf_to_supplier Data_lexer.lexer lexbuf

(*
let supplier lexbuf =
  let sup () =
    let (tok,_,_) as res = core_supplier lexbuf () in
    let () = Printf.printf "Token: \"%s\"\n%!" (tok_to_string tok) in
    res in
  sup
 *)

let supplier = core_supplier

let parse_data ?(overwrite=false) ?(output=false) filename includes env =
  try
    let in_ch =
      let fullname = Utils.find_file filename includes  in
      open_in fullname in
    let lexbuf = Lexing.from_channel in_ch in
    let () = Logs.app (fun m -> m "Parsing \"%s\"..." filename) in
    let starting_parse_time = Sys.time () in
    let e = (I.loop_handle succeed (fail lexbuf) (supplier lexbuf) (Data_parser.Incremental.main lexbuf.lex_curr_p)) ~overwrite env in
    let ending_parse_time = Sys.time () in
    let () = Logs.app (fun m -> m "Done (required %.3f seconds).\n%!" (ending_parse_time -. starting_parse_time)) in
    let () = match output with
	| false -> ()
	| true ->
	   Environment.iter 
	     (function 
	      | Environment.Signature sg -> 
		 Printf.printf "%s\n%!" (Environment.Signature1.to_string sg)
	      | Environment.Lexicon lex ->
		 Printf.printf "%s\n%!" (Environment.Lexicon.to_string lex))
	     e in
    Some e
  with
  | Utils.No_file(f,msg) ->
     let e =  Error.System_error (Printf.sprintf "No such file \"%s\" in %s" f msg) in 
     let () = Logs.err (fun m -> m "%s" (Error.error_msg e filename)) in
     None
  | Sys_error s ->
     let e = Error.System_error s in
     let () = Logs.err (fun m -> m "%s" (Error.error_msg e filename)) in
     None
  | Error.Error e -> 
     let () = Logs.err (fun m -> m "%s" (Error.error_msg e filename)) in
     None
	      
let pp_error er t = 
  let () = Utils.sformat "@." in    
  let _ = Format.flush_str_formatter () in
  let s,e = Error.get_loc_error er in
  let s',e' = s.Lexing.pos_cnum - s.Lexing.pos_bol,e.Lexing.pos_cnum - e.Lexing.pos_bol in
  let t_init = String.sub t 0 s' in
  let t_error = Utils.red (String.sub t s' (e'-s')) in
  let end_start_index  = (s' + (e'-s')) in
  let t_end = String.sub t end_start_index ((String.length t) - end_start_index) in
  let () = Logs.err (fun m -> m "%s%s%s" t_init t_error t_end) in
  Logs.err (fun m -> m "%s" (Error.error_msg er "stdin"))

let parse_term ?(output=false) t sg = 
  let lexbuf = Lexing.from_string t in
  try
    let abs_term,abs_type = I.loop_handle (fun x -> x) (fail lexbuf) (supplier lexbuf) (Data_parser.Incremental.term_alone lexbuf.lex_curr_p) sg in
    let () = 
      match output with
      | true -> 
	 let () = Utils.sformat "@[" in
	 let () = Utils.sformat "@[" in
	 let () = Environment.Signature1.term_to_formatted_string Format.str_formatter abs_term sg in
	 let () = Utils.sformat "@] :@ @[" in
	 let () = Environment.Signature1.type_to_formatted_string Format.str_formatter abs_type sg in
	 let () = Utils.sformat "@]" in
	 let expanded_term = Environment.Signature1.unfold abs_term sg in
	 let () =
	   if abs_term=expanded_term then
	     ()
	   else
	     let () = Utils.sformat "@,@ @[=@ " in
	     let () = Environment.Signature1.term_to_formatted_string Format.str_formatter expanded_term sg in Utils.sformat "@]"
	 in
	 let () = Utils.sformat "@]" in
	 ()
      | false -> () in
    Some (abs_term,abs_type)
  with
  | Error.Error er -> 
     let () = pp_error er t in
     None
  | End_of_file -> None



let parse_heterogenous_term  ?(output=false) t lex = 
  let lexbuf = Lexing.from_string t in
  let abs,obj=Environment.Lexicon.get_sig lex in
  try 
    let obj_term,abs_type =
      I.loop_handle (fun x -> x) (fail lexbuf)  (supplier lexbuf)  (Data_parser.Incremental.heterogenous_term_and_type lexbuf.lex_curr_p) abs obj in
    let abs_type=Environment.Signature1.convert_type abs_type abs in
    let obj_type=Environment.Lexicon.interpret_type abs_type lex in
    let obj_term=Environment.Signature1.typecheck obj_term obj_type obj in
    let () = match output with
      | true -> 
	 let () = 
	   Logs.app (fun m -> m
	                        "%s : %s (as image of %s)\n%!"
	                        (Environment.Signature1.term_to_string obj_term obj)
	                        (Environment.Signature1.type_to_string obj_type obj)
	                        (Environment.Signature1.type_to_string abs_type abs)) in
	 Logs.app (fun m -> m
	                      "%s : %s (as image of %s)\n%!"
	                      (Environment.Signature1.term_to_string (Environment.Signature1.unfold obj_term obj) obj)
	                      (Environment.Signature1.type_to_string obj_type obj)
	                      (Environment.Signature1.type_to_string abs_type abs))
      | false -> () in
    Some (obj_term,abs_type)
  with
  | Error.Error er -> 
     let () = pp_error er t in
     None
  | End_of_file -> None
                     
let parse_sig_entry t sg e = 
  let lexbuf = Lexing.from_string t in
  try
    Some (I.loop (supplier lexbuf) (Data_parser.Incremental.sig_entry_eoi lexbuf.lex_curr_p) sg e)
  with
  | Error.Error er -> 
     let () = pp_error er t in
     None
  | End_of_file -> None
                     
                     
let parse_lex_entry t lex e = 
  let lexbuf = Lexing.from_string t in
  try 
    Some (I.loop (supplier lexbuf) (Data_parser.Incremental.lex_entry_eoi lexbuf.lex_curr_p) lex e )
  with
  | Error.Error er -> 
     let () = pp_error er t in
     None
  | End_of_file -> None
                     
                     

