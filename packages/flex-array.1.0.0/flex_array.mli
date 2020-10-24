(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Flexible arrays.

    Flexible arrays are arrays whose size can be changed by adding or
    removing elements at either end (one at a time).

    This is an implementation of flexible arrays using Braun trees,
    following

      Rob Hoogerwoord
      A logarithmic implementation of flexible arrays
      http://alexandria.tue.nl/repository/notdare/772185.pdf

    All operations ([get], [set], [cons], [tail], [snoc], [liat])
    have logarithmic time complexity and logarithmic stack space.
*)

type +'a t
(** The type of flexible arrays.
    This is an immutable data structure.
    Values of type ['a t] can be compared using structural equality [=]
    (provided the elements of type ['a] can). *)

val empty: 'a t

val make: int -> 'a -> 'a t

val length: 'a t ->  int
(** Time complexity O(1) *)

val get: 'a t -> int -> 'a
(** [get a i] returns the element at index [i] in array [a].
    The first element has index 0.
    Raises [Invalid_argument] if [i] is outside the range 0
    to [length a - 1]. *)

val set: 'a t -> int -> 'a -> 'a t
(** [set a i v] returns a new array with all elements are identical to those
    of [a], except at index [i] where the element is [v].
    Raises [Invalid_argument] if [i] is outside the range 0
    to [length a - 1]. *)

val cons: 'a -> 'a t -> 'a t
(** [cons v a] returns a new array obtained by appending the value [v]
    at the front of array [a]. *)

val tail: 'a t -> 'a t
(** [tail a] returns a new array obtained by removing the value at the
    front of array [a].
    Raises [Invalid_argument] if [a] has length 0. *)

val snoc: 'a t -> 'a -> 'a t
(** [snoc v a] returns a new array obtained by appending the value [v]
    at the end of array [a]. *)

val liat: 'a t -> 'a t
(** [liat a] returns a new array obtained by removing the value at the
    end of array [a].
    Raises [Invalid_argument] if [a] has length 0. *)

(** {1 Iterators} *)

val iter: ('a -> unit) -> 'a t -> unit
(** [iter f a] applies function [f] in turn to all the elements of [a].
    It is equivalent to [f (get a 0); f (get a 1); ...;
    f (get a (n - 1); ()] where [n] is the length of the array [a],
    but runs faster.
    Time complexity O(n). Space complexity O(n). Constant stack space. *)

val iteri: (int -> 'a -> unit) -> 'a t -> unit
(** Same as {!iter}, but the
    function is applied with the index of the element as first argument,
    and the element itself as second argument. *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f x a] computes
    [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))],
    where [n] is the length of the array [a],
    but runs faster.
    Time complexity O(n). Space complexity O(n). Constant stack space. *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Same as {!fold}, but the
    function is applied with the index of the element as second argument. *)
