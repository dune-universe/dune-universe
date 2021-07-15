(* Time-stamp: <modified the 26/02/2015 (at 13:46) by Erwan Jahier> *)


(* nb: compiling = type checking + constant evaluation *)


(** This module (used to be called lazyCompiler) define an data
    structure made of tables storing lic entities (types, const,
    node). At the beginning (on creation), it only contains empty
    tables. But then, one when ask for a type, a const, or a node,
    the tables are filled in.
*)
type t

(** Create a lazy compiler. *)
val create : AstTab.t -> t

exception Recursion_error of Lv6Id.long * string list

(** Compiles one node (and update internal tables) *)
val compile_node : t -> Lv6Id.idref -> t

(** compile all items  (and update internal tables) *)
val compile_all : t -> t 

(** Just a simple change of data structure (from imperative tables to functional maps) *)
val to_lic_prg : t -> LicPrg.t 
