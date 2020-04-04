(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(** [('a, 'c) iter] is the conventional type of an [iter] function that
    iterates over a collection of type ['c] whose elements have type ['a]. *)

type ('a, 'c) iter =
  ('a -> unit) -> 'c -> unit

(** [('a, 'c) iter] is the conventional type of an [iteri] function that
    iterates over a collection of type ['c] whose elements have type ['a]. *)

type ('a, 'c) iteri =
  (int -> 'a -> unit) -> 'c -> unit

(** [('a, 'b, 'c) fold_left] is the conventional type of a [fold_left]
    function that iterates over a collection of type ['c] whose elements have
    type ['a] and that carries an accumulator of type ['b]. The accumulator is
    the first argument of the user function that represents the loop body. By
    convention, if the collection represents a sequence, then iteration should
    be performed left-to-right. *)

type ('a, 'b, 'c) fold_left =
  ('b -> 'a -> 'b) -> 'b -> 'c -> 'b

(** [('a, 'b, 'c) fold_right] is the conventional type of a [fold_right]
    function that iterates over a collection of type ['c] whose elements have
    type ['a] and that carries an accumulator of type ['b]. The accumulator is
    the second argument of the user function that represents the loop body. By
    convention, if the collection represents a sequence, then iteration should
    be performed right-to-left. *)

type ('a, 'b, 'c) fold_right =
  ('a -> 'b -> 'b) -> 'c -> 'b -> 'b

(** [fold_left] transforms an [iter] function, which should perform
    left-to-right iteration, into a [fold_left] function. *)

val fold_left:
  ('a, 'c) iter ->
  ('a, 'b, 'c) fold_left

(** [fold_right] transforms an [iter] function, which should perform
    right-to-left iteration, into a [fold_right] function. *)

val fold_right:
  ('a, 'c) iter ->
  ('a, 'b, 'c) fold_right

(** [iteri_left] transforms an [iter] function, which should perform
    left-to-right iteration, into an [iteri_left] function, which counts up
    from 0. *)

val iteri_left:
  ('a, 'c) iter ->
  ('a, 'c) iteri

(** [iteri_right] requires a [length] function and transforms an [iter]
    function, which should perform right-to-left iteration, into an
    [iteri_right] function, which counts down from [length s - 1]. *)

val iteri_right:
  ('c -> int) ->
  ('a, 'c) iter ->
  ('a, 'c) iteri

(** [('a, 'c) to_list] is the conventional type of a [to_list] function that
    converts a collection of type ['c], whose elements have type ['a], to a
    list. *)

type ('a, 'c) to_list =
  'c -> 'a list

(** [to_list] transforms an [iter_right] function, which should perform
    right-to-left iteration, into a [to_list] function. *)

val to_list: ('a, 'c) iter -> ('a, 'c) to_list

(** [('a, 'c) for_all] is the conventional type of a [for_all] function that
    iterates over a collection of type ['c] whose elements have type ['a]. *)

type ('a, 'c) for_all =
  ('a -> bool) -> 'c -> bool

val for_all: ('a, 'c) iter -> ('a, 'c) for_all
