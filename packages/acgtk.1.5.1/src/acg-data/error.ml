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

open UtilsLib

let update_loc lexbuf file =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
    | None -> pos.Lexing.pos_fname
    | Some s -> s
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
			     Lexing.pos_fname = new_file;
			     Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
			     Lexing.pos_bol = pos.Lexing.pos_cnum;
			 }


let infix_as_prefix = ref None
  
  
let set_infix l = infix_as_prefix := Some l
  
let unset_infix () = infix_as_prefix := None
  
let bad_infix_usage () =  !infix_as_prefix 


type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses of char
  | Unclosed_comment
  | Expect of string
  | Bad_token

type parse_error =
  | Syntax_error of string
  | Duplicated_term of string
  | Duplicated_type of string
  | Binder_expected of string
  | Unknown_constant of string
  | Not_def_as_infix of string
  | Unknown_constant_nor_variable of string
  | Unknown_constant_nor_type of string
  | Unknown_type of string
  | Missing_arg_of_Infix of string
  | No_such_signature of string
  | No_such_lexicon of string
  | Not_associative of string
  | Not_infix of string
  | Prefix_missing_arg of string
  | Infix_missing_first_arg of string
  | Infix_missing_second_arg of string

type type_error =
  | Already_defined_var of string
  | Not_defined_var of string
  | Not_defined_const of string
  | Not_well_typed_term of string * string
  | Not_well_typed_term_plus of string * string * string
  | Not_well_kinded_type of string
  | Non_linear_var of string
  | Linear_var of string
  | Other
  | Is_Used of string * string
  | Two_occurrences_of_linear_variable of (Lexing.position * Lexing.position)
  | Non_empty_context of (string*(Lexing.position * Lexing.position)*(Lexing.position * Lexing.position)*string)
  | Not_normal
  | Vacuous_abstraction of (string * (Lexing.position * Lexing.position))


type env_error =
  | Duplicated_signature of string
  | Duplicated_lexicon of string
  | Duplicated_entry of string

type version_error =  Outdated_version of (string*string)
    


type lexicon_error =
  | Missing_interpretations of (string * string * (string list))


type error = 
  | Parse_error of parse_error * (Lexing.position * Lexing.position)
  | Lexer_error of lex_error * (Lexing.position * Lexing.position)
  | Type_error of type_error * (Lexing.position * Lexing.position)
  | Env_error of env_error * (Lexing.position * Lexing.position)
  | Version_error of version_error
  | Lexicon_error of lexicon_error * (Lexing.position * Lexing.position)
  | System_error of string



type warning =
  | Variable_or_constant of (string * (Lexing.position * Lexing.position))

exception Error of error


let compute_comment_for_position pos1 pos2 =
  let line2 = pos2.Lexing.pos_lnum in
  let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
  let pos1 = pos1 in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "line %d, characters %d-%d" line2 col1 col2
    else
      Printf.sprintf "line %d, character %d to line %d, character %d" line1 col1 line2 col2


let lex_error_to_string = function
  | Unstarted_comment -> "Syntax error: No comment opened before this closing of comment"
  | Unstarted_bracket -> "Syntax error: No bracket opened before this right bracket"
  | Unclosed_comment -> "Syntax error: Unclosed comment"
  | Mismatch_parentheses c -> Printf.sprintf "Syntax error: Unclosed parenthesis '%c'" c
  | Expect s -> Printf.sprintf "Syntax error: %s expected" s
  | Bad_token -> "Lexing error: no such token allowed"

let parse_error_to_string = function
  | Syntax_error s -> Printf.sprintf "Syntax error: %s" s
  | Duplicated_type ty ->  Printf.sprintf "Syntax error: Type \"%s\" has already been defined" ty
  | Duplicated_term te ->  Printf.sprintf "Syntax error: Term \"%s\" has already been defined" te
  | Binder_expected id -> Printf.sprintf "Syntax error: Unknown binder \"%s\"" id
  | Unknown_constant id -> Printf.sprintf "Syntax error: Unknown constant \"%s\"" id
  | Not_def_as_infix id -> Printf.sprintf "Syntax error: \"%s\" is not an infix operator" id
  | Unknown_constant_nor_variable id -> Printf.sprintf "Syntax error: Unknown constant or variable \"%s\"" id
  | Unknown_constant_nor_type id -> Printf.sprintf "Syntax error: Unknown constant or type \"%s\"" id
  | Unknown_type id -> Printf.sprintf "Syntax error: Unknown atomic type \"%s\"" id
  | Missing_arg_of_Infix  id -> Printf.sprintf "Syntax error: \"%s\" is defined as infix but used here with less than two arguments" id
  | No_such_signature s -> Printf.sprintf "Syntax error: Signature id \"%s\" not in the current environment" s
  | No_such_lexicon s -> Printf.sprintf "Syntax error: Lexicon id \"%s\" not in the current environment" s
  | Not_associative s -> Printf.sprintf "Syntax error: Operator \"%s\" is not associative but is used without parenthesis" s
  | Not_infix s -> Printf.sprintf "Syntax error: Operator \"%s\" is not infix but is used as infix" s
  | Prefix_missing_arg s -> Printf.sprintf "Syntax error: The prefix operator \"%s\" is missing its argument" s
  | Infix_missing_first_arg s  -> Printf.sprintf "Syntax error: The infix operator \"%s\" is missing its first argument" s
  | Infix_missing_second_arg s  -> Printf.sprintf "Syntax error: The infix operator \"%s\" is missing its first argument" s

let type_error_to_string = function
  | Already_defined_var s ->
      Printf.sprintf "Var \"%s\" is already defined" s
  | Not_defined_var s -> 
      Printf.sprintf "Var \"%s\" is not defined" s
  | Not_defined_const s -> 
      Printf.sprintf "Const \"%s\" is not defined" s
  | Not_well_typed_term (s,typ) ->
      Printf.sprintf "Term \"%s\" not well typed.\nType expected : %s\n" s typ
  | Not_well_typed_term_plus (s,typ,wrong_typ) ->
      Printf.sprintf "Term \"%s\" not well typed.\n \"%s\" is of type %s but is here used with type  %s\n" s s typ wrong_typ
  | Not_well_kinded_type s ->
      Printf.sprintf "Type \"%s\" not well kinded" s
  | Non_linear_var s ->
      Printf.sprintf "Var \"%s\" is supposed to be non linear" s
  | Linear_var s ->
      Printf.sprintf "Var \"%s\" is supposed to be linear" s
  | Other -> "Not yet implemented"
  | Is_Used (s1,s2) -> Printf.sprintf "The type of this expression is \"%s\" but is used with type %s" s1 s2
  | Two_occurrences_of_linear_variable (s,e) -> Printf.sprintf "This linear variable was already used: %s" (compute_comment_for_position s e)
  | Non_empty_context (x,(s,e),funct_pos,funct_type) -> Printf.sprintf "This term contains a free linear variable \"%s\" at %s and is argument the term  of type \"%s\" at %s )" x (compute_comment_for_position s e)   funct_type (compute_comment_for_position (fst funct_pos) (snd funct_pos))
  | Not_normal -> "This term is not in normal form"
  | Vacuous_abstraction (x,(s,e)) -> Printf.sprintf "This linear variable \"%s\" is abstracted over but not used in term %s" x (compute_comment_for_position s e)

let env_error_to_string = function
  | Duplicated_signature s -> Printf.sprintf "Syntax error: Signature id \"%s\" is used twice in this environment" s
  | Duplicated_lexicon s -> Printf.sprintf "Syntax error: Lexicon id \"%s\" is used twice in this environment" s
  | Duplicated_entry s -> Printf.sprintf "Syntax error: Entry id \"%s\" is used twice in this environment" s

let lexicon_error_to_string = function
  | Missing_interpretations (lex_name,abs_name,missing_inters) ->
      Printf.sprintf "Lexicon definition error: Lexicon \"%s\" is missing the interpretations of the following terms of the abstract signature \"%s\":\n%s" lex_name abs_name (Utils.string_of_list "\n" (fun x -> Printf.sprintf"\t%s" x) missing_inters)

let version_error_to_string = function
  | Outdated_version (old_v,current_v) -> Printf.sprintf "You are trying to use an object file that was generated with a former version of the acgc compiler (version %s) while the current version of the compiler is %s" old_v current_v

let warning_to_string w input_file = 
  match w with
  | Variable_or_constant (s,(start,e)) ->
     Printf.sprintf "File \"%s\", %s\nWarning: %s"
                    input_file
                    (compute_comment_for_position start e)
                    (Printf.sprintf "\"%s\" is a variable here, but is also declared as constant in the signature" s)
	      
let error_msg e input_file =
  let msg,location_msg =
    match e with
      | Parse_error (er,(s,e)) -> parse_error_to_string er,Some (compute_comment_for_position s e)
      | Lexer_error (er,(s,e))  -> lex_error_to_string er,Some (compute_comment_for_position s e)
      | Type_error (er,(s,e)) -> type_error_to_string er,Some (compute_comment_for_position s e)
      | Env_error (er,(s,e)) -> env_error_to_string er,Some (compute_comment_for_position s e)
      | Version_error er ->  version_error_to_string er,None
      | Lexicon_error (er,(s,e)) -> lexicon_error_to_string er,Some (compute_comment_for_position s e)
      | System_error s -> Printf.sprintf "System error: \"%s\"" s,None in
    match location_msg with
      | None -> msg
      | Some loc -> Printf.sprintf "File \"%s\", %s\n%s" input_file loc msg

                                   (*
let dyp_error lexbuf =
  let pos1=Lexing.lexeme_start_p lexbuf in
  let pos2=Lexing.lexeme_end_p lexbuf in
    match bad_infix_usage () with
      | None -> Error (Parse_error (Dyp_error,(pos1,pos2)))
      | Some (sym,(s,e)) -> Error (Parse_error (Missing_arg_of_Infix sym,(s,e)))
                                    *)
(*
let emit_warning w input_file = 
  match w with
    | Variable_or_constant (_,(pos1,pos2)) -> 
	let msg = warning_to_string w input_file in
	let line2 = pos2.Lexing.pos_lnum in
	let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
	let pos1 = pos1 in
	let line1 = pos1.Lexing.pos_lnum in
	let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
	  if line1=line2 then
	    Printf.sprintf "File \"%s\", line %d, characters %d-%d\nWarning: %s"
	      input_file line2 col1 col2 msg
	  else
	    Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nWarning: %s"
	      input_file line1 col1 line2 col2 msg
 *)	      

let warnings_to_string input_file ws = Utils.string_of_list "\n" (fun w -> warning_to_string w input_file) ws
  
let get_loc_error = function
  | Parse_error (_,(s,e))
  | Lexer_error (_,(s,e))
  | Type_error (_,(s,e))
  | Env_error (_,(s,e))
  | Lexicon_error (_,(s,e)) -> (s,e)
  | (Version_error _ | System_error _) -> failwith "Bug: should not occur"
