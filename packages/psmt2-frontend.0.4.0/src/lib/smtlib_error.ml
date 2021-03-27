(******************************************************************************)
(*                                                                            *)
(* An SMT-LIB 2 for the Alt-Ergo Theorem Prover                               *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Format

type error =
  | Lexical_error of string
  | Syntax_error of string
  | Typing_error of string
  | Incremental_error of string
  | Unknow_Type_error of string
  | Missing_parameter_error of string
  | Logic_declaration_error of string
  | Sort_declaration_error of string
  | Datatype_declaration_error of string
  | Quantifier_error of string
  | Fun_declaration_error of string
  | Ambiguity_error of string
  | No_match_error of string
  | Type_clash_error of string * string

exception Error of error * ((Lexing.position * Lexing.position) option)

let report_loc fmt file (b,e) =
  let sfile =
    match file with
    | "" -> "; "
    | s -> sprintf "; File \"%s\"" s
  in
  if b = dummy_pos || e = dummy_pos then
    fprintf fmt "%s : " sfile
  else
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
    fprintf fmt "%s, line %d, characters %d-%d : " sfile l fc lc


let print fmt f e p =
  report_loc fmt f p;
  begin match e with
    | Lexical_error s -> fprintf fmt "Lexical error : %s" s
    | Syntax_error s -> fprintf fmt "Syntax error : %s" s
    | Typing_error s -> fprintf fmt "Typing error : %s" s
    | Incremental_error s -> fprintf fmt "Incremental error : %s" s
    | Unknow_Type_error s -> fprintf fmt "Unknown sort/type : %s" s
    | Missing_parameter_error s -> fprintf fmt "Missing parameter : %s" s
    | Logic_declaration_error s -> fprintf fmt "Logic declaration error : %s" s
    | Sort_declaration_error s -> fprintf fmt "Sort declaration error : %s" s
    | Fun_declaration_error s -> fprintf fmt "Fun declaration error : %s" s
    | Datatype_declaration_error s ->
      fprintf fmt "Datatypes declaration error : %s" s
    | Quantifier_error s -> fprintf fmt "Quantifier error : %s" s
    | Ambiguity_error s -> fprintf fmt "Ambiguity error : %s" s
    | No_match_error s -> fprintf fmt "No match for : %s" s
    | Type_clash_error(t1,t2) ->
      fprintf fmt "Clash type between : %s / %s" t1 t2
  end;
  fprintf fmt "@."

let error e p =
  (* print err_formatter "" e p; *)
  raise (Error (e,p))

let warning fmt e p =
  if Options.warning_as_error () then
    error e p
  else
    let p =
      match p with
      | None -> Lexing.dummy_pos,Lexing.dummy_pos
      | Some p -> p
    in
    print fmt (Options.filename ()) e p
