(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
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

(** This module gives some types and some utilities to mange, emit and
    display messages *)


(** The type for errors raised by the lexer. Names should be explicit
*)
type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses
  | Unclosed_comment
  | Expect of string
  | Bad_token

(** The type for errors raised by the parser. Names should be explicit
*)
type parse_error =
  | Duplicated_term of string
  | Duplicated_type of string
  | Binder_expected of string
  | Unknown_constant of string
  | Unknown_type of string
  | Missing_arg_of_Infix of string
  | No_such_signature of string
  | No_such_lexicon of string
  | Dyp_error

(** The types for errors raised by the typechecker. Names should be
    explicit *)
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


(** The types for errors raised by lexicons *)
type lexicon_error =
  | Missing_interpretations of (string * string * (string list))

(** The types for errors raised by the environment. Names should be
    explicit *)
type env_error =
| Duplicated_signature of string
| Duplicated_lexicon of string
| Duplicated_entry of string
    
type version_error =  Outdated_version of (string*string)
    
    
(** The type for errors *)
type error = 
| Parse_error of parse_error * (Lexing.position * Lexing.position)
| Lexer_error of lex_error * (Lexing.position * Lexing.position)
| Type_error of type_error * (Lexing.position * Lexing.position)
| Env_error of env_error * (Lexing.position * Lexing.position)
| Version_error of version_error
| Lexicon_error of lexicon_error * (Lexing.position * Lexing.position)
| System_error of string
    
(** The type for warnings *)
type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)

(** The exception that should be raised when an error occur *)
exception Error of error

(** [update_loc lexbuf name] update the position informations for the
    lexer *)
val update_loc : Lexing.lexbuf -> string option -> unit

(** [set_infix sym] declares sym as a prefix symbol used in an infix
    position *)
val set_infix : string * (Lexing.position * Lexing.position) -> unit

(** [unset_infix ()] unset the current use of a prefix symbol used in
    an infix position *)
val unset_infix : unit -> unit

(** [error_msg e filename] returns a string describing the error [e]
    while the file [filename] is being processed *)
val error_msg : error -> string -> string

(** [dyp_error lexbuf filename] returns an exception {!Error.Error} so
    that it can be caught in a uniform way. [lexbuf] and [filename] are
    used to set correctly the location information of the parse error *)
val dyp_error : Lexing.lexbuf -> string -> exn

(** [warnings_to_string filname ws] returns a string describing the
    warnings anf their location for the file [filename] *)
val warnings_to_string : string -> warning list -> string

(** [get_loc_error e] returns the starting and ending position of an
error *)
val get_loc_error : error -> (Lexing.position * Lexing.position)

val compute_comment_for_position : Lexing.position -> Lexing.position -> string
