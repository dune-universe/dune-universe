(** Structures *)

open Sym
open Formula
open Fact

type structure

(** Lookup with a predicate symbol *)
val lookup_pred : structure -> sym -> trie list

(** Lookup with a function symbol *)
val lookup_func : structure -> sym -> trie list

(* Lookup the universe as a unary predicate *)
val lookup_univ : structure -> trie list

(** Return the number of relations in the structure *)
val size : structure -> int

(** Return a sorted list of the facts *)
val facts : structure -> fact list

(** Make an empty structure *)
val mt_struct : unit -> structure

(** Augment a structure with new ground atoms *)
val augment_struct : structure -> atom list -> structure
