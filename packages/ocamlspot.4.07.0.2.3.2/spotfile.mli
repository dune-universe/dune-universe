(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Spot
open Spoteval

exception Old_cmt of string * string
val load : load_paths:string list -> string -> Unit.t
val load_module : ?spit:bool -> cwd:string -> load_paths:string list -> string -> Unit.t (* CR jfuruse: spit *)
val load_directly_with_cache : string -> Unit.t

val initial_env   : Unit.t -> Env.t
val invalid_env : Unit.t -> Env.t

type result = File_itself | Found_at of string * Region.t | Predefined

val find_path_in_flat : Unit.t -> Kind.t * Path.t -> PIdent.t * result
val str_of_global_ident : cwd:string -> load_paths:string list -> Ident.t -> string * Value.structure
val eval_packed : Env.t -> string -> Value.t

