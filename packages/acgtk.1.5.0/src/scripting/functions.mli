(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
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

val info : Unix.file_descr -> string -> unit
open AcgData.Environment


module type Action_sig = 
sig

  type env
  type context

  (*  exception Not_yet_implemented of string*)
  exception Stop
  exception Quit

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Check
    | Realize
    | RealizeShow
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save
    | Parse
    | Idb
    | Query
    | Exit

  type file_type =
  | Data
  | Object
  | Script of (string -> context * env -> context * env)

  val color_output : bool -> unit

  val set_config : string -> string list -> unit
    
  val load : file_type -> string -> context * env -> context * env

  val list : env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : (Lexing.position * Lexing.position) -> unit
  val dont_trace : (Lexing.position * Lexing.position) -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val check : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val realize : ?names:(string * (Lexing.position * Lexing.position)) list -> ?svg_output:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val realize_show : ?names:(string * (Lexing.position * Lexing.position)) list -> ?svg_output:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val parse : ?name:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val idb : ?name:string -> env ->  (Lexing.position * Lexing.position) -> unit

  val query : ?name:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val add : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> env

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> env -> env

  val context : wait:bool -> echo:bool -> svg:string option -> dirs:string list -> parse_fun:(string -> context -> Environment.t -> context * Environment.t) -> context

  val wait : context -> context

  val dont_wait : context -> context

  val should_wait : context -> bool

  val echo : context -> bool

  val svg : context -> string option

  val dirs : context -> string list

  val parse_script : context -> (string -> context -> Environment.t -> context * Environment.t)

  val help : action -> unit

  val exit : unit -> unit


  val create_sig :  (string * (Lexing.position * Lexing.position)) -> env -> env


  val create_lex :  abs:(string * (Lexing.position * Lexing.position)) -> obj:(string * (Lexing.position * Lexing.position)) -> (string * (Lexing.position * Lexing.position)) -> env -> env

  val save : ?names:(string * (Lexing.position * Lexing.position)) list -> string -> env -> (Lexing.position * Lexing.position) -> unit


end

module Functions : Action_sig with type env=AcgData.Environment.Environment.t
