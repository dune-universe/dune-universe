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

val info : Unix.file_descr -> string -> unit
open AcgData.Environment


module type Action_sig = 
sig

  type env
  type context

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

  val color_output : context -> bool -> context
    
  val set_config : context -> string -> string list -> context

  val resize : context -> bool
    
  val load : file_type -> string -> context * env -> context * env

  val list : context -> env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : (Lexing.position * Lexing.position) -> unit
  val dont_trace : (Lexing.position * Lexing.position) -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : context -> ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val check : context -> ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val realize : context -> ?names:(string * (Lexing.position * Lexing.position)) list -> ?svg_output:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val realize_show : ?names:(string * (Lexing.position * Lexing.position)) list -> ?svg_output:string -> context -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val parse : context -> ?name:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val idb : ?name:string -> env ->  (Lexing.position * Lexing.position) -> unit

  val query : ?name:string -> context -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val add : ?names:(string * (Lexing.position * Lexing.position)) list -> context -> env -> string -> (Lexing.position * Lexing.position) -> env

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> env -> env

  val make_context : wait:bool -> colored_output:bool -> pretty_printed_output:bool -> echo:bool -> svg:string option -> dirs:string list -> rendering_config:Rendering_config.config -> parse_fun:(string -> context -> Environment.t -> context * Environment.t) -> context

  val wait : context -> context

  val dont_wait : context -> context

  val should_wait : context -> bool

  val echo : context -> bool

  val svg : context -> string option

  val dirs : context -> string list

  val parse_script : context -> (string -> context -> Environment.t -> context * Environment.t)

  val help : context -> action -> unit

  val exit : unit -> unit


  val create_sig :  (string * (Lexing.position * Lexing.position)) -> env -> env


  val create_lex :  abs:(string * (Lexing.position * Lexing.position)) -> obj:(string * (Lexing.position * Lexing.position)) -> (string * (Lexing.position * Lexing.position)) -> env -> env

  val save : ?names:(string * (Lexing.position * Lexing.position)) list -> string -> env -> (Lexing.position * Lexing.position) -> unit


end

module Functions : Action_sig with type env=AcgData.Environment.Environment.t
