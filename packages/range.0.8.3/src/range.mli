(* SPDX-License-Identifier:  GPL-3.0-or-later *)

open Base
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

type t
(** t type correspond to an integer range value *)

type elt = Int.t

module Number : sig
  type 'a t

  val gtz_from_int : Int.t -> [ `Greater_than_zero ] t Option.t

  val gtz_from_int_exn : Int.t -> [ `Greater_than_zero ] t

  val positive_from_int : int -> [ `Positive ] t Option.t

  val positive_from_int_exn : int -> [ `Positive ] t

  val to_int : 'a t -> int
end

(**  'a number type is an integer with constraints **)

include Equal.S with type t := t

include Stringable.S with type t := t

val from : elt -> elt -> t
(** from start_value stop_value : will create a t value representing the range
    described by the two values given in parameter.

    @param start Integer representating the starting value of the range
    @param stop Integer representing the last value of the range
    @return Range.t type which value defined by start and stop parameters

 *)

val filter : t -> f:(elt -> bool) -> t
(** filter f range : will create a new Range.t value using predicate function
    f. This modifies the behaviour of iter or fold function in order to apply
    only to values that satisfies the predicate.

    @param ~f the predicate is attached to the range value, the predicate
    must respect the signature int -> bool
    @param range range to be filtered, if the range provided has already a filter, the new
    range value will merge the two filters.
    @return new Range.t value with a new filter added.
    *)

val reset : t -> t
(** remove all map and filter effects from a range.

    @param old Range.t value
    @return new Range.t value from parameter without modifiers.
   *)

val is_natural : t -> bool
(** is filtered predicate

    test if a Range.t value contain a filter or map function transforming data.
    @param Range.t value to test
    @return test true if there is a filter false otherwise
   *)

val length : t -> Int.t
(**
length range_value : return the number of elements contained in rang_value

    @param range_value : range_value of type t
    @return Int.t with the number of elements
*)

val fold : 'a -> t -> f:('a -> elt -> 'a) -> 'a
(** fold the equivalent of List.fold_left applied to integer range_record
    explore all the values contained in the range value applying f to the
    accumulator and the current element read by fold. If a filter was associated
    to the range value, only element validated by the predicate f will be passed
    to the function.

    @param f function aggregating the accumulator to the current value.
    @param acc initial value of the accumulator
    @param range explored range value
    @return value of the accumulator after reading all elements *)

val fold_right : 'a -> t -> f:('a -> elt -> 'a) -> 'a
(** fold_right explore all the values contained in the range value, in reverse order,
    starting by the last value to the first one, applying f to the accumulator and
    the current element read by fold. If a filter was associated to the range value,
    only element validated by the predicate f will be passed to the function.

    @param f function aggregating the accumulator to the current value.
    @param acc initial value of the accumulator
    @param range explored range value
    @return value of the accumulator after reading all elements *)

val iter : t -> f:(elt -> unit) -> unit
(** iter apply a function with side effect on all values of the range. This
    function support filtering.

    @param f function receiving an integer and returning unit
    @param range value
    @return unit
   *)

val split :
  [ `Greater_than_zero ] Number.t ->
  [ `Greater_than_zero ] Number.t ->
  t ->
  t list
(** split a range value into a list of smaller range, useful for batching in
    parallel processing.

    @param minimal size of a range
    @param count number of subranges contained in the list.
    @param range value to split
    @return list of ranges with a size of minimal or greater, the list having
    count elements max.
    *)

val contain : elt -> t -> bool
(** contain function to test if an integer value is contained in a Range.t values

    @param element to be tested
    @param reference range value
    @return true if element is contained in reference
    *)

val cross : t -> t -> t Option.t
(** new Range.t value representing the common value between two Range.t values.

    @param a Range.t value
    @param b Range.t value
    @return option value with Range.t option type value defined by the common
    values, None if it is impossible to find common values.
    *)

val cross_exn : t -> t -> t
(** Same as cross function with exception for error handling.
   *)

val join : t -> t -> t Option.t
(** Join to generate a new Range.t value contained both a and b

    @param a Range.t value
    @param b Range.t value
    @return Range.t option value containing both a and b.
    If a and b are disjoint, they can't be joinded so None is returned.
   *)

val join_exn : t -> t -> t
(** Same as join with exception for error handling
   *)

val map : t -> f:(elt -> elt) -> t
(** apply f to elements contained in a Range.t value

    This feature uses a delayed application of f. Like for filters, f is stacked
    on previous filter or map functions.

    @param f function applied to range contents
    @param r range to modify
    @return updated range
   *)
