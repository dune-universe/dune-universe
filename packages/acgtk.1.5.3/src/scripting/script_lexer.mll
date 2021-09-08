(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2021 INRIA                             *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

{
  open AcgData
  open Command_parser

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
  let string_content = Buffer.create 16

  let echo_content = Buffer.create 32
    
  let space_added = ref true

(*  let no_trailing_space_regexp =
    Str.regexp "^[ \t]*\\([^ \t].*[^ \t]\\)[ \t]*"

  let strip_trailing_space s = 
    let () = Printf.fprintf stderr "looking at \"%s\"\n%!" s in
    if Str.string_match no_trailing_space_regexp s 0 
    then
      Str.matched_group 1 s
    else
      failwith "Bug: matched string expected" *)

  let strip_trailing_space s = String.trim s

  let extract = function
    | None -> "help"
    | Some x -> x

  let add_space () =
    let () = if !space_added then () else Buffer.add_char echo_content ' ' in
      space_added := false
	
	
  let echo_str s = let () = add_space () in Buffer.add_string echo_content s
  let echo_chr s = let () = add_space () in Buffer.add_char echo_content s
  let reset_echo () = 
    let s = Buffer.contents echo_content in
    let () = Buffer.reset echo_content in
    let () = space_added := true in
      s
      
}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*
  let skip_space = ([' ' '\t' '\n'])*
  let help_command = skip_space "help" skip_space ";"


  rule lexer =
  parse
    | [' ' '\t'] {lexer lexbuf}
    | newline {let () = Error.update_loc lexbuf None in lexer lexbuf}
    | eof {EOII}
    | "#" {comment lexer lexbuf}
    | [';'] as c {let () = echo_chr c  in let s = reset_echo () in SEMICOLONN s}
    | "load" as c {let () = echo_str c in let t = load_options lexbuf in t}
    | "create" as c {let () = echo_str c in let t = create_options lexbuf in t}
    | "list"  as c {let () = echo_str c in LIST}
    | "select"  as c {let () = echo_str c in SELECT}
    | "unselect"  as c {let () = echo_str c in UNSELECT}
    | "trace"  as c {let () = echo_str c in TRACE (loc lexbuf)}
    | "help"  as c {let () = echo_str c in HELP}
    | "print"  as c {let () = echo_str c in PRINT (loc lexbuf)}
    | "analyse"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string (fun x l -> match x with
		| None -> ANALYSE_HELP
		| Some x -> ANALYSE (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
      | "check"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string (fun x l -> match x with
		| None -> CHECK_HELP
		| Some x -> CHECK (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
      | "realize"  as c {let () = echo_str c in
                         let () = Buffer.reset string_content in
		         optional_string (fun x l ->
                             match x with
		             | None -> REALIZE_HELP
		             | Some x -> REALIZE (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
      | "realize_show"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string (fun x l -> match x with
		| None -> REALIZE_SHOW_HELP
		| Some x -> REALIZE_SHOW (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "parse"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string (fun x l -> match x with
		| None -> PARSE_HELP
		| Some x -> PARSE (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "idb"  as c {let () = echo_str c in IDB (loc lexbuf)}
    | "query"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string (fun x l -> match x with
		| None -> QUERY_HELP
		| Some x -> QUERY (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "add"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		optional_string_wo_space (fun x l -> match x with
		| None -> ADD_HELP
		| Some x -> ADD (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "compose"  as c {let () = echo_str c in COMPOSE}
    | "don't"  as c {let () = echo_str c in DONT (loc lexbuf)}
    | "wait"  as c {let () = echo_str c in WAIT}
    | "exit"  as c {let () = echo_str c in EXIT}
    | "as"  as c {let () = echo_str c in AS}
    | "save" as c {let () = echo_str c in let () = Buffer.reset string_content in
		     optional_string_wo_space (fun x l -> match x with
		| None -> SAVE_HELP
		| Some x -> SAVE (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | letter string  as c {let () = echo_str c in IDENTT (Lexing.lexeme lexbuf,loc lexbuf)}
  (*    | _ {raise (Scripting_errors.Error (Scripting_errors.Command_expected,loc lexbuf))} *)
  and comment f_parser = parse
    | newline {let () = Error.update_loc lexbuf None in f_parser lexbuf}
    | _ {comment f_parser lexbuf}
  and optional_string f = parse
      | [' ' '\t'] {optional_string f lexbuf} 
      | "help" {let () = echo_str ("help") in
		 f None (loc lexbuf)}
      | _ as c {let () = Buffer.add_char string_content c in
                let start_pos,_ = loc lexbuf in
                string (fun x (_,e) -> f x (start_pos,e)) lexbuf} 
  and string f = parse
    | ";" {f (Some (Buffer.contents string_content)) (loc lexbuf)}
    | "#" {comment (string f) lexbuf}
    | newline {let () = Error.update_loc lexbuf None in string f lexbuf}
    | _ as c {let () = Buffer.add_char string_content c in string f lexbuf}
  and optional_string_wo_space f = parse
      | [' ' '\t'] {optional_string f lexbuf} 
      | "help" {let () = echo_str ("help") in
		f None (loc lexbuf)}
      | _ as c {let () = Buffer.add_char string_content c in
                let start_pos,_ = loc lexbuf in
                string_wo_space (fun x (_,e) -> f x (start_pos,e)) lexbuf}
  and string_wo_space f = parse
    | ";" {f (Some (Buffer.contents string_content)) (loc lexbuf)}
    | "#" {comment (string_wo_space f) lexbuf}
    | [' ' '\t'] {string_wo_space f lexbuf}
    | newline {let () = Error.update_loc lexbuf None in string_wo_space f lexbuf}
    | _ as c {let () = Buffer.add_char string_content c in string f lexbuf}
  and load_options = parse
    | [' ' '\t'] {load_options lexbuf}
    | newline {let () = Error.update_loc lexbuf None in load_options lexbuf}
    | eof {EOII}
    | "help" {LOAD_HELP}
    | "#" {comment load_options lexbuf}
    | "data" as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_DATA (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "d" as c {let () = echo_chr c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_DATA (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "object" as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_OBJECT (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "o" as c {let () = echo_chr c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_OBJECT (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "script" as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_SCRIPT (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "s" as c {let () = echo_chr c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> 
		  let x = extract x in LOAD_SCRIPT (strip_trailing_space x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
  (*    | _ {raise (Scripting_errors.Error (Scripting_errors.Missing_option Scripting_errors.Load,loc lexbuf))} *)
  and create_options = parse
    | [' ' '\t'] {create_options lexbuf}
    | "help" {CREATE_HELP}
    | newline {let () = Error.update_loc lexbuf None in create_options lexbuf}
    | eof {EOII}
    | "#" {comment create_options lexbuf}
    | "s" as c {let () = echo_chr c in let () = Buffer.reset string_content in CREATE_SIG}
    | "sig" as c {let () = echo_str c in let () = Buffer.reset string_content in CREATE_SIG}
    | "l" as c {let () = echo_chr c in let () = Buffer.reset string_content in CREATE_LEX}
    | "lex" as c {let () = echo_str c in let () = Buffer.reset string_content in CREATE_LEX}



