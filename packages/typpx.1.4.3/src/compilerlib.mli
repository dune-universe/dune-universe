open Ppxx.Utils

module Ident : sig
  include module type of struct include Ident end
      
  val format : Format.formatter -> t -> unit

  val format_verbose : Format.formatter -> t -> unit
  (** Prints also stamp integers *)    
end
  
module Path : sig
  include module type of struct include Path end

  val format : Format.formatter -> t -> unit
  val format_verbose : Format.formatter -> t -> unit
  val to_string : t -> string
end

module Ctype : module type of struct include Ctype end
(**
  ctype.ml says:

   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].

  Therefore we simply wrap these functions by correct_levels here.
  They may be slower but I do not want to be bothered by strange
  type level bugs.
*)

module Types : sig
  include module type of struct include Types end
      
  val repr_desc : type_expr -> type_desc
  (** repr + desc *)

  val expand_repr_desc : Env.t -> type_expr -> type_desc
  (** expand_head + repr + desc *)

  val with_snapshot : (unit -> 'a) -> 'a
  (** Run the given function. Unifications caused by the function are undo-ed. *)

  val is_constr : Env.t -> type_expr -> (Path.t * type_expr list) option
  (** Check the type is a Tconstr *)

  val is_option_type : Env.t -> type_expr -> type_expr option
  (** Check the type is option *)

  val gen_vars : type_expr -> type_expr list
  (** Generalized tvars *)

  val create_uniq_type : unit -> type_expr
  (** Create a unique data type. Note that the result data type lacks definition. *)

  val close_gen_vars : type_expr -> unit
  (** Unify genvars with unique data types.  Use with_snapshot to recover the original types *)
end
