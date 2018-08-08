(** Extensions of compiler-libs *)
open Migrate_parsetree.Ast_405
open Utils

module Longident : sig
  include module type of struct include Longident end

  val format : Format.t -> t -> unit
  val to_string : t -> string
end
  
module Ident : sig
  include module type of struct include Ident end
      
  val format : Format.formatter -> t -> unit

  val format_verbose : Format.formatter -> t -> unit
  (** Prints also stamp integers *)    
end
  
module Path : sig
  include module type of struct include Path end

  val format : Format.formatter -> t -> unit

  val to_string : t -> string
  val format_verbose : Format.formatter -> t -> unit
  (** Prints also stamp integers *)    
end
  
module Location : sig
  include module type of struct include Location end
      
  val format : Format.formatter -> t -> unit

  val merge : t -> t -> t
  (** [merge t1 t2] returns a location which starts at the start of [t1]
      and ends at the end of [t2].
  *)
end

module XParsetree : sig
  open Parsetree

  val iter_core_type : (core_type -> unit) -> core_type -> unit
  (** Iteration on core_type, like Btype.iter_type_expr *)

  val constrs_in_core_type : core_type -> Longident.t list
  (** Referred constrs and classes *)

  val constrs_in_type_declaration : type_declaration -> Longident.t list
  (** Referred constrs and classes *)

  val is_gadt : type_declaration -> bool
  (** Return [true] if it defines a GADT *)

  val sccs : ('v * 'v list) list -> 'v list list

  val group_type_declarations
    : type_declaration list -> type_declaration list list * type_declaration list
  (** Group the type declarations defined by one [type] without [nonrec].
      The result consists of the two components.
      The first is the truly mutual type declaration groups.  The type
      declarations of each group are defined truly mutual recursively.
      The second is the the type declarations not really recursively defined.

      For example,
        type t = Foo of u
        and u = { label : t; label2 : v; }
        and v = Zee of v
        and w = Boo
      the function returns the following: 
        ([ [t; u]; [v] ], [w])
  *)
end

(** The followings are so common in PPX *)

val raise_errorf: ?loc:Location.t -> ?sub:Location.error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Same as Location.raise_errorf *)

type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }
(** Same as Location.loc *)
