(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

(** Range module provide a type for handling the description of an integer
sequence described by a start value and a stop value. This module provide
functions to fold, filter and map this range.

The main goal is to :
    *   provide a split capacity in order to make life easy for
    distributed processing.
    *   avoid the use of list or lazy list when the only need is to sequence a
    range of integer.

@author Aldrik KLEBER <contact@aldrik.net>
*)
open Base
type t
type elt=int
(** t type correspond to a integer range value *)

include Equal.S with type t := t
include Stringable.S with type t := t

val from : elt -> elt -> t
(** from start_value stop_value : will create a t value representing the range
    described by the two values given in parameter.

    @param start Integer representating the starting value of the range
    @param stop Integer representing the last value of the range
    @return Range.t type which value defined by start and stop parameters

 *)

val filter : (elt -> bool) -> t -> t
(** filter f range : will create a new Range.t value using predicate function
    f. This modifies the behaviour of iter or fold funtions
    in order to apply only to values that satisfies the predicate.

    @param f the predicate is attached to the range value, the predicate
    must respect the signature int -> bool
    @param range range to be filtered, if the range provided has already a filter, the new
    range value will merge the two filters.
    @return new Range.t value with a new filter added.
    **)

val reset : t -> t
(** remove all map and filter effects from a range.

    @param old Range.t value
    @return new Range.t value from parameter without modifiers.
   **)

val is_natural : t -> bool
(** is filtered predicate

    test if a Range.t value contain a filter or map function transforming data.
    @param Range.t value to test
    @return test true if there is a filter false otherwise
   **)

val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
(** fold the equivalent of List.fold_left applied to integer range_record
    explore all the values contained in the rang value applying f to the
    accumulator and the current element read by fold. If a filter was associated
    to the range value, only element validated by the predicate f will be passed
    to the function.

    @param f function aggregating the accumulator to the current value.
    @param acc initial value of the accumulator
    @param range explored range value
    @return value of the accumulator after reading all elements **)

val iter : (elt -> unit) -> t -> unit
(** iter apply a function with side effect on all values of the range. This
    function support filtering.

    @param f function receiving an integer and returning unit
    @param range value
    @return unit
   **)

val split : elt -> elt -> t -> t list
(** split a range value into a list of smaller range, useful for batching in
    parallel processing.

    @param minimal size of a range
    @param count number of subranges contained in the list.
    @param range value to split
    @return list of ranges with a size of minimal or greater, the list having
    count elements max.
    **)

val contain : elt -> t -> bool
(** contain function to test if an integer value is contained in a Range.t values

    @param element to be tested
    @param reference range value
    @return true if element is contained in reference
    **)

val cross : t -> t -> t Option.t
(** new Range.t value representing the common value between two Range.t values.

    @param a Range.t value
    @param b Range.t value
    @return option value with Range.t option type value defined by the common
    values, None if it is impossible to find common values.
    **)

val cross_exn : t -> t -> t
(** Same as cross function with exception for error handling.
   **)

val join : t -> t -> t Option.t
(** Join to generate a new Range.t value contained both a and b

    @param a Range.t value
    @param b Range.t value
    @return Range.t option value containing both a and b.
    If a and b are disjoint, they can't be joinded so None is returned .
   **)

val join_exn : t -> t -> t
(** Same as join with exception for error handling
   **)

val map : (elt -> elt) -> t -> t
(** apply f to elements contained in a Range.t value

    This feature used a delayed application of f. Like for filters, f is stacked
    on previous filter or map functions.

    @param f function to apply to the content of a range value
    @param r range to modify
    @return updated range
   **)

val to_string : t -> string
(** export limits of a range to a string.

    The presence of functions that can modifiy the content is signaled by "M:"
    prefix

    @param r Range.t value to export in a string .
    @return string representing the content of r Range.t value.
   **)
