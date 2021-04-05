(* Copyright 2016 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: Apache-2.0 *)

(** Lazily-realized lists. *)

(** {1 Representation} *)

type 'a t = 'a node Lazy.t

and 'a node = Nil | Cons of 'a * 'a t

(** {1 Printing} *)

val pp :
     sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
(** Pretty-print a lazy list with items separated by [sep]. This will traverse
    the entire list, so printing an infinite or very long list is inadvisable. *)

(** {1 Construction} *)

val items : 'a list -> 'a t
(** [items xs] is a lazy list consisting of the items in the list [xs]. *)

val of_array : 'a array -> 'a t
(** [of_array xs] is a lazy list consisting of the items in the array [xs]. *)

val unit : 'a -> 'a t
(** [unit a] constructs a lazy list consisting only of the item [a]. *)

(** {1 Generation}

    These functions are used to generate lazy lists from specifications. *)

val fill : int -> 'a -> 'a t
(** [fill n a] generates a lazy list consisting of [n] instances of the item
    [a]. *)

val unfold : 's -> ('s -> ('s * 'b) option) -> 'b t
(** State-based generation.

    Starting with an initial state, generate a new state and a value.
    Termination is indicated by an outcome of [None]. *)

val iterate : 'a -> ('a -> 'a) -> 'a t
(** Generate an infinite lazy list by repeatedly iterating on a value. *)

val continually : 'a -> 'a t
(** [continually a] generates an infinite lazy list consisting of the item [a]. *)

val enum_from : int -> int t
(** [enum_from z] generates an infinite lazy list consisting of the integers
    beginning at [z] and incremented by one at each step. *)

val enum_from_to : int -> int -> int t
(** Like {!enum_from}, but the result is a finite lazy list that terminates at
    the upper bound. *)

val cycle : 'a t -> 'a t
(** Generate an infinite lazy list consisting of cycles of the argument. *)

(** {1 Manipulation} *)

val head : 'a t -> 'a option
(** The first item in a lazy list, or [None] if the list is empty. *)

val tail : 'a t -> 'a t
(** Everything but the first item in a lazy list. This is the dual of {!head}.

    In the case of an empty lazy list, the result is {!Nil}. *)

val take : int -> 'a t -> 'a t
(** [take n t] is a lazy list consisting of the first [n] items in [t]. *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** [take_while f t] is a lazy list consisting of the first items of [t] that
    satisfy the predicate [f]. The first item in [t] that does not satisfy the
    predicate terminates the sequence. *)

val drop : int -> 'a t -> 'a t
(** [drop n t] is the lazy list [t] without its first [n] items. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while f t] is the lazy list [t] without the first items that satisfy
    the predicate [f]. The first item in [t] that does not satisfy the predicate
    terminates the sequence. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f t] is the lazy list [t] with the [f] applied to each of its items. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** [flat_map f t] is equivalent to [map f t |> flatten]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f t] is the lazy list [t] with only the items that satisfy the
    predicate [f]. *)

val map_filter : ('a -> 'b option) -> 'a t -> 'b t
(** [map_filter f t] applies [f] to each element in [t] and produces a new lazy
    list consisting of the non-[None] values. *)

val flatten : 'a t t -> 'a t
(** Flatten a lazy list of lazy lists. *)

(** {1 Querying} *)

val exists : ('a -> bool) -> 'a t -> bool
(** Check for the existence of an item in the lazy list that satisfies a
    predicate.

    The first item to satisfy the predicate terminates the realization of the
    list. *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Check that all items in the lazy list satisfy the predicate.

    This requires realizing the entirety of the structure. *)

val find : ('a -> bool) -> 'a t -> 'a option
(** Return the first item in the lazy list that satisfies the predicate. *)

(** {1 Combining} *)

val concat : 'a t -> 'a t -> 'a t
(** Combine two lazy lists into a single lazy list, one after the other. *)

val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Merge two lazy lists together with a generating function. *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Merge two lazy lists into a lazy list of pairs. *)

val zip_all_with : ('a option -> 'b option -> 'c) -> 'a t -> 'b t -> 'c t
(** Like {!zip_with}, but merge elements from differently sized lazy lists. *)

val zip_all : 'a t -> 'b t -> ('a option * 'b option) t
(** Like {!zip}, but merge elements from differently sized lazy lists. *)

(** {1 Folding} *)

val strict : 'a t -> 'a list
(** Realize a lazy list into a strict list. *)

val fold_right : ('a -> 'b Lazy.t -> 'b) -> 'a t -> 'b Lazy.t -> 'b
(** Fold over a lazy list from the right with an accumulation function.

    Since the folding function is lazy in the second argument, evaluation of the
    lazy list can be short-circuited. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold over a lazy list from the left with an accumulation function. *)

val length : _ t -> int
(** The number of elements in the lazy list. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Compare two lazy lists for equality with an element comparison function. *)

(** {1 Iterating} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f t] applies the function [f] to each item in the lazy list. This only
    makes sense when [f] performs stateful effects. *)
