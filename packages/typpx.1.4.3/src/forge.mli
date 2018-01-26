(** This module provides convenient functions to build Typedtree AST.
    This does not cover all the construction... yet.
*) 

open Asttypes
open Typedtree
open Types
   
val default_loc : Location.t ref

val with_loc : Location.t -> (unit -> 'a) -> 'a
(** Set [default_loc] and run a function *)
    
val loc : 'a -> 'a Location.loc
  
module Dummy : sig

  (** Dummy builder. The data is set to some default and you need to override
      some of the fields. *)
    
  val type_expr : type_expr
  val env : Env.t
  val value_description : unit -> value_description
  val exp_desc : expression_desc
  val exp : unit -> expression
  val mod_type : module_type
  val structure_item : unit -> structure_item
end
  
val lidentloc_of_path : Path.t -> Longident.t Location.loc
  
module Path : sig
  type t = Path.t
  val of_lident : Longident.t -> t
end
  
module Typ : sig
  open Types
  val arrow : ?label:arg_label -> type_expr -> type_expr -> type_expr
end

module Exp : sig

  val untyped : Parsetree.expression -> expression
  (** [untyped] is to embed an untyped AST in an typed AST. The embeded untyped AST
      will be kept as is when the typed one is untyped.
  *)
    
  val ident : Path.t -> expression

  val let_ :
    ?recursive:bool ->
    value_binding list ->
    expression -> expression

  val letmodule :
    Ident.t ->
    module_expr -> expression -> expression

  val app :
    expression ->
    (arg_label * expression) list -> expression

  val ignore : expression -> expression
  (** [ignore e] creates [Pervasives.ignore <e>]. 
      No check of [Pervasives] is the really OCaml stdlib's [Pervasives].
  *)

  val fun_ : ?label:arg_label -> pattern -> expression -> expression

  val tuple : expression list -> expression

  val with_env : Env.t -> expression -> expression
  (** Override expression's type environment field *)

  val none : ?ty: type_expr -> Env.t -> expression
  (** Build [None] of the given content type. If [ty] is omitted
      the container type is [Dummy.type_expr].

      Raises [Assert_failure] when [None] is not accessible in the environment.
  *)
    
  val some : Env.t -> expression -> expression
  (** Build [Some e] of the given expression.
      Raises [Assert_failure] when [Some] is not accessible in the environment.
  *)

  val list : Env.t -> expression list -> expression
  (** Build the list of given expressions. The container type is 
      [t list] where [t] is the type of the first expression.
      If no type is given, [Dummy.type_expr] is used.

      Raises [Assert_failure] when either [(::)] and [[]] is 
      not accessible in the environment.
  *)
    
  val mark : string -> expression -> expression
  (** Add [@<string>] to the expression *)

  val partition_marks : expression -> (string -> bool) -> string list * expression
  (** Filter out matching [@<string>] attributes from the given expression. *)
end
  
module Pat : sig
  val desc : pattern_desc -> pattern
  val var : Ident.t -> pattern
end

module MB : sig
  val module_binding :
    Ident.t -> module_expr -> module_binding
end

module Mod : sig
  val of_module_expr_desc :
    module_expr_desc -> module_expr
  val ident : Path.t -> module_expr
  val unpack : expression -> module_expr
end
