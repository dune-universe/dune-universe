(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_data.mli,v 1.2 2004/07/30 13:09:43 barnier Exp $ *)

(** {1 Bactrackable Data Structures} *)

(** This module provides "efficient" backtrackable data structures,
   i.e. with incremental setting and trailing. *)

module Array : sig
  val set : 'a array -> int -> 'a -> unit
  (** [set t i x] Bactrackable assignment of [t.(i)] with [x]. *)
end
(** Bactrackable arrays. *)

module Hashtbl : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val get : ('a, 'b) t -> ('a, 'b) Hashtbl.t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
end
(** Bactrackable hashtables.
   This module provides a subset of the hashtable interface
   of the OCaml standard library module Hashtbl (see {% ~\cite{ocaml}%}). *)
