(* Time-stamp: <modified the 26/02/2015 (at 13:44) by Erwan Jahier> *)

(** Maps ident to entities (const, type et oper) in some particular contexts. *)

(** Une AstTabSymbol.t est une association ``contextuelle'' entre
des ident simples (string) et ce à quoi ils correspondent.
    
    N.B. il est inutile de gérer les idents absolus (pack+nom) qui sont
    par définition NON-CONTEXTUELS.  Dans une table, on a 3 espaces de
    noms, un par nature d'item (const, type et oper).

***********************************************************)
open AstCore
open Lxm

(** Symbol table elements. *)
type 'a elt =
  | Local of 'a
  | Imported of Lv6Id.long * static_param srcflagged list

type t

val create : unit -> t

(** Manip de AstTabSymbol.t *)

(** Raise a proper compil error message if not found *)
val find_type :  t -> Lv6Id.t -> Lxm.t -> (type_info  Lxm.srcflagged) elt
val find_const : t -> Lv6Id.t -> Lxm.t -> (const_info Lxm.srcflagged) elt
val find_node :  t -> Lv6Id.t -> Lxm.t -> (node_info  Lxm.srcflagged) elt

val find_pack_of_type  : t -> Lv6Id.t -> Lxm.t -> Lv6Id.pack_name
val find_pack_of_const : t -> Lv6Id.t -> Lxm.t -> Lv6Id.pack_name

(** Ajout de nom d'item importés (via uses) *)
val add_import_const : t -> Lv6Id.pack_name -> Lv6Id.t -> Lv6Id.long -> unit
val add_import_type  : t -> Lv6Id.t -> Lv6Id.long -> unit
val add_import_node  : t -> Lv6Id.t -> Lv6Id.long -> static_param srcflagged list -> unit

(** Add local items declaration *)
val add_type  : t -> Lv6Id.pack_name -> Lv6Id.t -> type_info  Lxm.srcflagged -> unit
val add_const : t -> Lv6Id.pack_name -> Lv6Id.t -> const_info Lxm.srcflagged -> unit
val add_node  : t -> Lv6Id.t -> node_info  Lxm.srcflagged -> unit

(** Itérer sur les items *)

val iter_types: t -> (Lv6Id.t -> (type_info Lxm.srcflagged) elt -> unit) -> unit
val iter_consts: t ->(Lv6Id.t -> (const_info Lxm.srcflagged) elt -> unit) -> unit
val iter_nodes : t ->(Lv6Id.t -> (node_info Lxm.srcflagged) elt -> unit) -> unit


