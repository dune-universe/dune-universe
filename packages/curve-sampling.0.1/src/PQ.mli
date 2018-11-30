(* File: curve_sampling_pq.mli

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type 'a t
(** Mutable maximum priority queue, with float priority. *)

type 'a witness
(** A value witness that enables to increase its priority or remove it
   from the priority queue. *)

val make : unit -> 'a t
(** [make()] returns an empty priority queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] tells whether the queue [q] is empty. *)

val add : 'a t -> float -> 'a -> unit
(** [add q p x] add [x] with priority [p] to [q].
    @raise Invalid_argument if [p] is NaN. *)

val witness_add : 'a t -> float -> 'a -> 'a witness
(** [witness_add q p x] does the same as {!add} and in addition return
   a witness for [x]. *)

val max : 'a t -> 'a
(** [max q] returns an element of [q] with maximum priority.
    @raise Failure if the queue is empty. *)

val max_priority : 'a t -> float
(** [max_priority q] returns the maximum priority of elements in [q]
    or [neg_infinity] if [q] is empty.  *)

val delete_max : 'a t -> 'a
(** [delete_max q] delete an element with maximum priority from [q]
    and return it.

    @raise Failure if the queue is empty. *)

val priority : 'a witness -> float
(** [priority w] returns the priority of the element witnessed by [w]. *)

val increase_priority : float -> 'a witness -> unit
(** [increase_priority p w] set the priority of the value pointed by
   the witness [w] to [p] (in the queue in which the value is).  If
   the new priority is lower than the previously given one, this
   function does nothing. *)

val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
(** [fold q init f] folds the function [f] on all elements present in
   the queue [q].  The order in which elements are passed is
   unspecified. *)

val foldi : 'a t -> init:'b -> f:('b -> float -> 'a -> 'b) -> 'b
(** [foldi q init f] same as {!fold} but [f] also receive the priority. *)

val iter : 'a t -> f:('a -> unit) -> unit
(** [iter q f] iterates the function [f] on all elements present in
    the queue [q] (which is unchanged).  The order in which elements
    are passed is unspecified. *)

val iteri : 'a t -> f:(float -> 'a -> unit) -> unit
(** [iteri q f] same as {!iter} but [f] also receive the priority. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** [map q f] return a new priority queue with the same priority
   structure than [q] but with [f x] instead of each data value [x]. *)

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
(** [filter_map q f] Same as [map] be remove the values for which [f]
   returns [None]. *)

;;
