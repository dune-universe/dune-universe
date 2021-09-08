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

open AcgData
open Logic.Abstract_syntax

type command =
  | Load

type error =
  | Missing_option of command
  | Not_in_environment of string
  | No_such_lexicon of string
  | Command_expected
  | Not_yet_implemented of string
  | No_focus
  | Accept_only of data_type * string
  | Parsing of string
and  data_type = 
  | Lex of string
  | Sg of string

exception Error of (error * Abstract_syntax.location * string option)

let error_msg er (s,e) filename =
  let file_info =
    match filename with
    | None -> ""
    | Some s -> Printf.sprintf "File \"%s\" " s in
  let loc = Error.compute_comment_for_position s e in
  let msg = match er with
    | Missing_option Load -> "Option (\"data\" or \"d\" or \"script\" or \"s\") is missing to the load command" 
    | Not_in_environment s -> Printf.sprintf "No %s entry in the current environment" s 
    | No_such_lexicon  s -> Printf.sprintf "No lexicon \"%s\" in the current environmnet" s 
    | Command_expected -> "Command expected" 
    | No_focus -> "No data on which to apply the command"
    | Accept_only (Lex s,cmd) -> Printf.sprintf "The %s command can only apply to lexicons. Here it is applied to a signature: \"%s\"" cmd s
    | Accept_only (Sg s,cmd) -> Printf.sprintf "The %s command can only apply to signatures. Here it is applied to a lexicon: \"%s\"" cmd s
    | Not_yet_implemented s -> Printf.sprintf "\"%s\": Command not yet implemented" s
    | Parsing s -> s  in
  Printf.sprintf "%s%s:\n%s\n%!" file_info loc msg
