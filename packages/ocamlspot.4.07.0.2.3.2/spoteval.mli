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
open Utils

module PIdent : sig
  (** Identifier with file name path *)
  type t = { path : string;
             ident : Ident.t option; }
  val format : Format.formatter -> t -> unit
end

module Value : sig

  type t =
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option
    | Closure of PIdent.t * env * Ident.t * Types.module_type option * Abstraction.module_expr
    | Parameter of PIdent.t
    | Error of exn

  and structure = structure_item list

  and structure_item = Ident.t * (Kind.t * t Lazy.t)

  and z = t Lazy.t

  and env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : binding;
  }
      
  and binding (** = Binding.t *)

  (** functions which force lazy parts of the given values *)
  module Enforcer : functor (A : sig  end) -> sig
    val t : t -> unit
    val env : env -> unit
    val binding : binding -> unit
    val structure : structure -> unit
    val structure_item : structure_item -> unit
    val z : z -> unit
  end

  module Format : sig
    open Format
    val t : formatter -> t -> unit
    val env : formatter -> env -> unit
    val binding : formatter -> binding -> unit
    val structure : formatter -> structure -> unit
    val z : formatter -> z -> unit
  end
end

module Binding : sig
  type t = Value.binding
      
  val domain : t -> Ident.t list
  val find : t -> Ident.t -> (Kind.t * Value.z) option
  val override : t -> Value.structure_item -> t
  val overrides : t -> Value.structure -> t
  val set : t -> Value.structure -> unit
  val predef : t
  val empty : t
  val invalid : t
end
  
module Env : sig
  type t = Value.env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : Binding.t;
  }
  val format : Format.formatter -> Value.env -> unit
  val domain : t -> Ident.t list
  val find : t -> Ident.t -> (Kind.t * Value.z) option
  val override : t -> Value.structure_item -> t
  val overrides : t -> Value.structure -> t
  val predef : t
end

module Eval : sig
  (** These function ref will be initialized from ocamlspot.ml *)
  val str_of_global_ident :
    (cwd:string -> load_paths:string list -> Ident.t -> string * Value.structure) ref
  val packed : (Env.t -> string -> Value.t) ref
    
  val find_path : Env.t -> Kind.t * Path.t -> Value.z
    
  val find_ident : Value.structure -> Kind.t * string * int -> Value.z

  val module_expr :
    Env.t ->
    Ident.t option ->
    Abstraction.module_expr -> Value.z

  val structure : Env.t -> Abstraction.structure -> Value.structure
    
  val apply : Value.z -> Value.z -> Value.z
end
