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
(* $Id: scheduling.mli,v 1.3 2004/08/09 16:04:41 barnier Exp $ *)

open Facile
open Easy

module Task : sig
  type t
  (** Type of tasks. *)
  val create : ?name:string -> Fd.t -> int -> t
  (** [create start processing_time] return a new task with start time
   [start] and constant duration [processing_time]. *)
  val name : t -> string
  (** [name t] return the name of task [t]. *)
  val start : t -> Fd.t
  (** [start t] return the start time of task [t] as a variable. *)
  val end_time : t -> Arith.t
  (** [end_time t] return the end time of task [t] as an expression
     (i.e. [start t +~ processing_time t]). *)
  val release_date : t -> int
  (** [release_date t] return the earliest start time of task [t]
     (i.e. [Fd.min (start t)]). *)
  val processing_time : t -> int
  (** [processing_time t] return the duration of task [t]. *)
  val deadline : t -> int
  (** [name t] return the latest completion time of task [t]
     (i.e. [Fd.max (start t) + processing_time t]). *)
  val before : t -> t -> Cstr.t
  (** [before t1 t2] returns a constraint ensuring that [t1] is
     processed before [t2]. *)
  val after : t -> t -> Cstr.t
  (** [after t1 t2] returns a constraint ensuring that [t1] is
     processed after [t2]. *)
  val update_release_date : t -> int -> unit
  (** [update_release_date t start] refine task [t] so that it
     cannot begin before [start]. *)
  val update_deadline : t -> int -> unit
  (** [update_deadline t endtime] refine task [t] so that it
     cannot end after [endtime]. *)
  val fprint : out_channel -> t -> unit
  (** [fprint chan t] print task [t] on out channel [chan]. *)
end

type t
(** Type of unary ressource. They are associated with the set
   of tasks which require it. *)
val fprint : out_channel -> t -> unit
(** [fprint chan r] print ressource [r] (i.e. the set of tasks
   which require it) on out channel [chan]. *)
val create : Task.t list -> t
(** [create ts] return a new ressource which must process (if constrained)
   all tasks in list [ts]. *)
val tasks : t -> Task.t list
(** [tasks r] return the list of tasks that require [r]. *)
val iter : (Task.t -> unit) -> t -> unit
(** [iter f r] iterate on all tasks requiring ressource [r]. *)
val number_of_tasks : t -> int
(** [number_of_tasks r] return the number of tasks requiring
   ressource [r]. *)
val edge_finding : t -> Cstr.t
(** [edge_finding r] return a unary capacity constraint on
   ressource [r]. Propagations are performed with the edge-finding
   bounding algorithm. *)
val disjunctive : t -> Cstr.t
(** [disjunctive r] return a unary capacity constraint on
   ressource [r]. Propagations are performed by pairwise
   disjunctive constraints on all the tasks of [r]. *)

module Goals : sig
  val rank : t -> Goals.t
  (** [rank r] return a goal that ranks tasks of ressource [r]:
     the unranked task with the smallest release date is
     non-deterministically chosen to be ranked first. *)
end
