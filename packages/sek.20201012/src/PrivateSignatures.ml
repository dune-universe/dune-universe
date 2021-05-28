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

open PublicTypeAbbreviations

(* -------------------------------------------------------------------------- *)

(** The exception [Empty] is raised by [pop] and [peek] operations. *)
exception Empty

(** The exception [End] is raised by the iterator operation [get]. *)
exception End

(* -------------------------------------------------------------------------- *)

(** A point of view is either [Front] or [Back]. It serves as a parameter to
    the operations that can operate at either end of a sequence. *)

(** When passed as a parameter to an [iter] function, [Front] means
    left-to-right iteration, whereas [Back] means right-to-left iteration. *)
type pov =
  | Front
  | Back

let[@inline] dual pov =
  match pov with
  | Front -> Back
  | Back -> Front

let[@inline] sign pov =
  match pov with
  | Front -> +1
  | Back  -> -1

let[@inline] exchange pov this that =
  match pov with
  | Front ->
      this, that
  | Back ->
      that, this

let show_pov = function
  | Front ->
      "Front"
  | Back ->
      "Back"

(* Public names for these types, constants, and functions. *)

type side = pov
let front = Front
let back  = Back
let other = dual

type direction = pov
let forward  = Front
let backward = Back
let sign = sign
let opposite = dual

(* -------------------------------------------------------------------------- *)

(** The type [weight] represents the weight of an element or the total
    weight of a sequence of elements. It is a nonnegative integer. *)
type weight = int

(** A function of type ['a update] is a function that knows how to update
    an element in a sequence. Its arguments are the element [x] that must
    be updated and the weight-index at which [x] must be updated, if it
    is itself a sequence. (One might say that the function does not
    represent a single update but a weight-indexed family of updates.) *)
type 'a update =
  'a -> weight -> 'a

(* -------------------------------------------------------------------------- *)

(** The module [EphemeralChunk] implements an ephemeral chunk, that is,
    a mutable circular buffer. This is its signature. *)
module type ECHUNK = sig

  (** A chunk is a mutable object, which represents a sequence of elements of
      type ['a]. A chunk has a fixed capacity, which it cannot exceed. *)
  type 'a t

  (** [check c] verifies that the chunk [c] is valid, i.e., that its internal
      invariant is satisfied. *)
  val check : 'a t -> unit

  (** [create d n] creates a fresh chunk whose capacity is [n]. The default
      element [d] is used to initialize empty slots. If [overwrite_empty_slots]
      is [true], then it is also used to overwrite a slot that becomes empty,
      e.g., in [pop] and [carve_back]. *)
  val create : 'a -> capacity -> 'a t

  (** [make d n k v] creates a fresh chunk whose default element is [d], whose
      capacity is [n], and which contains a sequence of [k] copies of the value
      [v]. The integer [k] must be no greater than [n]. *)
  val make : 'a -> capacity -> length -> 'a -> 'a t

  (** [init d n k i f] creates a fresh chunk whose default element is [d],
      whose capacity is [n], and which contains the sequence of values produced
      by the calls [f i], [f (i+1)], ... [f (i+k-1)], in this order. The
      integer [k] must be no greater than [n]. *)
  val init : 'a -> capacity -> length -> index -> (index -> 'a) -> 'a t

  (** [dummy d] creates a dummy (invalid!) chunk, which can be used as a
      default element when creating a chunk of chunks. This dummy chunk is
      invalid and must not be used. As an exception to this rule, it is
      permitted to apply [is_empty] and [is_full] to a dummy chunk; in that
      case, they both return [true]. *)
  val dummy : 'a -> 'a t

  (** [is_dummy c] tells whether [c] is a dummy chunk, that is, whether it
      has been built by [dummy]. *)
  val is_dummy : 'a t -> bool

  (** [of_array_segment d n a head size] creates a chunk by copying data from
      the array segment defined by the array [a], the start index [head], and
      the size [size]. The chunk has capacity [n] and default value [d].
      [size] must be less than or equal to [n]. *)
  val of_array_segment : 'a -> capacity -> 'a array -> index -> length -> 'a t

  (** [default c] returns the default element that was provided when the
      chunk [c] was created. *)
  val default : 'a t -> 'a

  (** [length c] is the length of the sequence [c]. *)
  val length : 'a t -> length

  (** [capacity c] is the capacity of the chunk [c], that is, the maximum
      number of elements that this chunk can hold. *)
  val capacity : 'a t -> capacity

  (** [data c] is the raw array that underlies the chunk [c]. This function
      is dangerous; it should be used only to implement efficient iterators. *)
  val data : 'a t -> 'a array

  (** [is_empty c] is equivalent to [length c = 0]. *)
  val is_empty : 'a t -> bool

  (** [is_full c] is equivalent to [length c = capacity c]. *)
  val is_full : 'a t -> bool

  (** [is_empty_or_dummy c] is equivalent to [is_empty c || is_dummy c]. *)
  val is_empty_or_dummy : 'a t -> bool

  (** [is_full_or_dummy c] is equivalent to [is_full c || is_dummy c]. *)
  val is_full_or_dummy : 'a t -> bool

  (** [get c i] returns the element found at index [i] in the sequence [c].
      [i] must be comprised between 0 included and [length c] excluded. *)
  val get : 'a t -> index -> 'a

  (** [set c i x] replaces the element found at index [i] in the sequence [c]
      with the value [x]. [i] must be comprised between 0 included and
      [length c] excluded. *)
  val set : 'a t -> index -> 'a -> unit

  (** [peek pov c] returns the first or last element of the chunk [c],
      depending on the point-of-view [pov]. The chunk [c] must be nonempty. *)
  val peek : pov -> 'a t -> 'a

  (** [push pov c x] inserts the element [x] at the front or back of the
      chunk [c], depending on the point-of-view [pov]. The chunk [c] must
      not be full. *)
  val push : pov -> 'a t -> 'a -> unit

  (** [pop pov c] extracts and returns the first or last element of the
      chunk [c], depending on the point-of-view [pov]. The chunk [c] must
      be nonempty. *)
  val pop : pov -> 'a t -> 'a

  (** [copy c] creates and returns a copy of the chunk [c]. The copy is
      identical to the original chunk, but is disjoint from [c], so an
      update to the copy does not affect [c]. *)
  val copy : 'a t -> 'a t

  (** [clear c] updates the chunk [c] so that it represents the empty
      sequence. *)
  val clear : 'a t -> unit

  (** [carve_back c i] splits the sequence [c] at index [i]. The index [i]
      must be comprised between 0 included and [length c] included. After this
      operation, the chunk [c] is truncated and represents the first part of
      the sequence, up to index [i] excluded. A new chunk, which represents the
      second part of the sequence, beginning at index [i], is returned. *)
  val carve_back : 'a t -> index -> 'a t

  (** [take c i] truncates the sequence [c] at index [i]. The index [i]
      must be comprised between 0 included and [length c] included. After this
      operation, the chunk [c] is truncated and represents the first part of
      the sequence, up to index [i] excluded. *)
  val take : 'a t -> index -> unit

  (** [drop c i] truncates the sequence [c] at index [i]. The index [i]
      must be comprised between 0 included and [length c] included. After this
      operation, the chunk [c] is truncated and represents the second part of
      the sequence, beginning at index [i]. *)
  val drop : 'a t -> index -> unit

  (** [print] is a chunk printer, parameterized with an element printer.
      It is intended to be used only while debugging. *)
  val print : ('a -> PPrint.document) -> 'a t -> PPrint.document

  module View : sig

    (** A view is an immutable object, which represents a certain range in a
        chunk. It can be thought of as a pair of a [head] index and a [size]. *)
    type view

    (** A view is normally obtained via the function [view], whose declaration
       comes after the end of the submodule [View]. *)

    (** [head v] is the head of the view [v], that is, the index in the
        underlying chunk of the first element of the view [v]. *)
    val head : view -> index

    (** [size v] is the size of the view [v], that is, the number of
        elements in this view. *)
    val size : view -> length

    (** [check c v] verifies that the view [v] is valid with respect to the
        chunk [v], i.e., that its internal invariant is satisfied. *)
    val check : 'a t -> view -> unit

    (** [dummy] is a dummy view. It may be invalid and must not be used in
        any way. *)
    val dummy : view

    (** [iter_segments pov (c, v) f] returns a sequence of up to two array
        segments that cover the view [v] of the chunk [c]. *)
    val iter_segments : pov -> 'a t * view -> 'a segments

    (** [peek pov c v] returns the first or last element of the view [v]
        in chunk [c], depending on the point-of-view [pov]. The view [v]
        must be nonempty. *)
    val peek : pov -> 'a t -> view -> 'a

    (** [get c v i] returns the element found at index [i] in the view [v]
        of the sequence [c]. [v] must be a valid view with respect to [c],
        and [i] must be comprised between 0 included and [size v] excluded. *)
    val get : 'a t -> view -> index -> 'a

    (** [set c v i x] replaces the element found at index [i] in the view [v]
        of the sequence [c] with [x]. [v] must be a valid view with respect to
        [c], and [i] must be comprised between 0 included and [size v]
        excluded. *)
    val set : 'a t -> view -> index -> 'a -> unit

    (** [three_way_split c v i] returns the element [x] found at index [i] in
        the view [v] of the sequence [c], along with two views that cover
        the elements of [v] found left and right of [x]. [v] must be a valid
        view with respect to [c], and [i] must be comprised between 0
        included and [size v] excluded. *)
    val three_way_split : 'a t -> view -> index -> view * 'a * view

    (** [take] is a specialized version of [three_way_split]. Instead of
        returning a triple [view1, x, view2], it returns a pair [view1, x]. *)
    val take : 'a t -> view -> index -> view * 'a

    (** [drop] is a specialized version of [three_way_split]. Instead of
        returning a triple [view1, x, view2], it returns a pair [x, view2]. *)
    val drop : 'a t -> view -> index -> 'a * view

    (** [is_flush pov c v] determines whether the front (resp. back) of the
        view [v] coincides with the front (resp. back) of the chunk [c]. *)
    val is_flush : pov -> 'a t -> view -> bool

    (** [is_aligned c v] determines whether the view [v] covers all of the
        sequence [c]. It is equivalent to [is_flush Front c v &&
        is_flush Back c v]. *)
    val is_aligned : 'a t -> view -> bool

    (** [push pov c v] is the view obtained by pushing one element in front
        of (resp. at the back of) the view [v]. The view [v] must not
        cover all of [c]. *)
    val push : pov -> 'a t -> view -> view

    (** [pop pov c v] is the view obtained by popping one element in front
        of (resp. at the back of) the view [v]. The view [v] must be
        nonempty. *)
    val pop : pov -> 'a t -> view -> view

    (** [restrict c v i k] is the view obtained by restricting the view [v] to
        the subview defined by the relative index [i] and the length [k]. *)
    val restrict : 'a t -> view -> index -> length -> view

    (** [copy pov c1 v1 c2 v2] copies the data in the view [v1] of chunk
        [c1] to the front (resp. back) of view [v2] of [c2], depending on
        the point-of-view [pov]. The view [v1] and [v2] must be valid with
        respect to the chunks [c1] and [c2], respectively. There must be enough
        room for the data, that is, [size v1 + size v2 <= capacity c2] must
        hold. The function returns a view that is valid with respect to [c2]
        and includes [v2] plus the newly-copied data. *)
    val copy : pov -> 'a t -> view -> 'a t -> view -> view

    (** [sub c v] creates a new chunk that contains the data covered by the
        view [v] of the chunk [c]. The copied data is located in view [v]
        of the new chunk [c'], so [Chunk.view c'] is [v]. *)
    val sub : 'a t -> view -> 'a t

    (** [iter pov f (c, v)] applies the function [f] in turn to every element
        in view [v] of chunk [c], in the order specified by the point-of-view
        [pov]. *)
    val iter : pov -> ('a -> unit) -> 'a t * view -> unit

    (** [fold_left f (c, v) seed] applies the function [f] in turn to every
        element in view [v] of chunk [c], while maintaining an accumulator. *)
    val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t * view -> 'b

    (** [segment pov i k c v] returns the description of a segment of at most
        [k] consecutive elements, starting from the one at index [i] and
        progressing in the direction [pov]. The index [i] is relative to the
        view [v]: it must be in the interval [\[0, size v)]. The length [k]
        must be no greater than the number of elements between index [i]
        (included) and the end of the view [v] in the direction of iteration.

        [segment Front i k c v] returns a triple [(a, j, m)] such that the
        elements of the view [v] in the interval [\[i, i + m)] are stored in
        the array [a] in the segment [\[j, j + m)].

        [segment Back i k c v] returns a triple [(a, j, m)] such that the
        elements of the view [v] in the interval [(i - m, i\]] are stored in
        the array [a] in the segment [\[j, j + m)].

        The value [m] is the maximal value such that [m <= k] and the elements
        starting at index [i] in the direction of iteration are stored
        consecutively in memory. If [k > 0] holds, then it is guaranteed that
        [m > 0] holds as well.

        In the current implementation, all elements of a view can be covered
        by using at most two segments. Thus, after [segment Front i k c v]
        returns [(a, j, m)], either [m = k] holds and all elements of the view
        have been covered, or calling [segment Front (i + k) (k - m) c] returns
        a segment that covers all of the remaining elements of the view. *)
    val segment : pov -> index -> length -> 'a t -> view -> 'a segment

    (** [print] is a view printer. It is intended to be used only while
        debugging. *)
    val print : view -> PPrint.document

  end (* View *)

  (** [view c] is a view that covers all of the elements currently
      found in the chunk [c]. *)
  val view : 'a t -> View.view

  (** [iter_segments pov c] returns a sequence of up to two array segments
      that cover the chunk [c]. It is analogous to [View.iter_segments] above,
      but applies to the entire chunk (not to a view). It is permitted to apply
      [iter_segments] to a dummy chunk. *)
  val iter_segments : pov -> 'a t -> 'a segments

end

(* -------------------------------------------------------------------------- *)

(** The type [owner] represents the owner of a shareable chunk. *)

type owner =
  Owner.owner

(* -------------------------------------------------------------------------- *)

(** The module [ShareableChunk] implements a shareable chunk, that is, a chunk
    that is either owned by a single owner or shared between multiple owners.
    This is its signature. *)
module type SCHUNK = sig

  module EChunk : ECHUNK
  type 'a chunk = 'a EChunk.t
  type view = EChunk.View.view

  (** A shareable chunk, also known as a schunk, represents a sequence of
      elements of type ['a]. Like a chunk, it has a fixed capacity, which
      it cannot exceed. *)

  (** A schunk keeps track of the total weight of the elements that it
      contains. An element does not necessarily have weight one; a measure [m],
      which must be passed by the user to many operations, indicates how much
      each element weighs. (This will not be repeated in the specification of
      every operation.) *)

  (** A schunk is either shared or uniquely owned. When it is shared, it
      cannot be modified. When it is uniquely owned, it can be modified. As far
      as the user is concerned, this is transparent: the user does not decide
      whether to allocate a new schunk or modify an existing schunk in place.

      However, the user is responsible for presenting an appropriate owner
      identity [o] when creating or using a schunk. (This will not be repeated
      in the specification of every operation.)

      A schunk records the identity of its creator. It is considered uniquely
      owned by its creator unless its creator is [Owner.none], in which case
      it is considered shared. Later on, when an operation is performed on this
      schunk with identity [o], the function [Owner.is_uniquely_owned] is used
      to compare the schunk's creator with [o]. The outcome of this test
      determines whether the schunk is uniquely owned or shared, therefore can
      or cannot be modified in place. *)
  type 'a t

  (** A measure, also known as a weight function, maps an element to a weight.
      Many of the functions in the modules [ShareableChunk] and
      [ShareableSequence] are parameterized with a measure. *)

  (** Being able to test whether a measure is the unit measure -- which maps
      every element to a weight of one -- is useful. This allows optimizations
      in some functions, such as [reach]. For this reason, instead
      of defining the type ['a measure] as a synonym for ['a -> weight], we
      define it as a (generalized) algebraic data type. *)
  type 'a measure =
  | MUnit         : 'a measure   (* maps every value to 1 *)
  | MSWeight : 'a t measure (* maps a schunk to its weight *)

  (** [apply m x] applies the measure [x] to the element [x] and returns its
      weight. *)
  val apply : 'a measure -> 'a -> weight

  (** [check m o p] verifies that the schunk [p] is valid, i.e., that its
      internal invariant is satisfied. This check is relative to the measure
      [m] and to the identity [o] with which this schunk is accessed. *)
  val check : 'a measure -> owner -> 'a t -> unit

  (** [is_uniquely_owned p o] determines whether the schunk [p] is uniquely
      owned by owner [o]. *)
  val is_uniquely_owned : 'a t -> owner -> bool

  (** [default p] returns the default element that was provided when the
      schunk [p] was created. *)
  val default : 'a t -> 'a

  (** [length p] is the length of the sequence [p]. A dummy schunk has
      length 0. *)
  val length : 'a t -> length

  (** [weight p] is the total weight of the elements of the sequence [p]. *)
  val weight : 'a t -> weight

  (** [data p] is the raw array that underlies the chunk [p]. This function
      is dangerous; it should be used only to implement efficient iterators. *)
  val data : 'a t -> 'a array

  (** [capacity p] is the capacity of the chunk [p], that is, the maximum
      number of elements that this chunk can hold. *)
  val capacity : 'a t -> capacity

  (** [is_empty p] is equivalent to [length p = 0]. *)
  val is_empty : 'a t -> bool

  (** [is_full p] is equivalent to [length p = capacity p]. *)
  val is_full : 'a t -> bool

  (** [create d n v] creates a schunk with default element [d], capacity [n],
      and owner [o]. *)
  val create : 'a -> capacity -> owner -> 'a t

  (** [dummy d] creates a dummy (invalid!) schunk. *)
  val dummy : 'a -> 'a t

  (** [is_dummy p] tells whether [p] is a dummy schunk, that is, whether it
      has been built by [dummy]. *)
  val is_dummy : 'a t -> bool

  (** [support p] returns the chunk that supports the schunk [p]. *)
  val support : 'a t -> 'a chunk

  (** [copy p o] creates a copy of the schunk [p]. The copy is owned by [o]. *)
  val copy : 'a t -> owner -> 'a t

  (** [of_chunk_destructive c o] creates a new schunk, based on the ephemeral
      chunk [c], with owner [o]. The ownership of the chunk [c] is lost. The
      elements of the chunk [c] are assumed to have unit weight. *)
  val of_chunk_destructive : 'a chunk -> owner -> 'a t

  (** [to_chunk p o] converts the schunk [p] to an ephemeral chunk. If [p] is
      uniquely owned by [o], then it is destroyed and its support is re-used
      directly; no copy is required. If [p] is shared, then a copy is performed
      and a new chunk is allocated. Like [of_chunk], this operation assumes
      that every element has unit weight. *)
  val to_chunk : 'a t -> owner -> 'a chunk
  (* TODO: rename [to_chunk] to [to_chunk_destructive] *)

  (** [of_array_segment d n a head size o] creates a schunk with owner [o] by
      copying data from the array segment defined by the array [a], the start
      index [head], and the size [size]. The schunk has capacity [n] and
      default value [d]. [size] must be less than or equal to [n]. The elements
      are assumed to have unit weight. *)
  val of_array_segment :
    'a -> capacity -> 'a array -> index -> length -> owner -> 'a t

  (** [make d n k v o] creates a schunk that contains [k] copies of the value
      [v]. The schunk has default value [d], capacity [n], and owner [o]. The
      elements are assumed to have unit weight. *)
  val make: 'a -> capacity -> length -> 'a -> owner -> 'a t

  (** [init d n k i f o] creates a fresh schunk that contains the sequence of
      values produced by the calls [f i], [f (i+1)], ... [f (i+k-1)], in this
      order. The schunk has default value [d], capacity [n], and owner [o]. The
      elements are assumed to have unit weight. *)
  val init: 'a -> capacity -> length -> index -> (index -> 'a) -> owner -> 'a t

  (** [peek pov p] returns the first or last element of the schunk [p],
      depending on the point-of-view [pov]. [p] must be nonempty. *)
  val peek : pov -> 'a t -> 'a

  (** [push pov p x m v] pushes the element [x] at the front (resp. back) of
      the schunk [p], depending on the point-of-view [pov]. [p] must not be
      full. Either [p] is updated in place, or a new schunk is allocated;
      this is transparent. *)
  val push : pov -> 'a t -> 'a -> 'a measure -> owner -> 'a t

  (** [pop pov p m v] pops an element off the front (resp. back) of the schunk
      [p], depending on the point-of-view [pov]. [p] must not be empty. Either
      [p] is updated in place, or a new schunk is allocated; this is
      transparent. *)
  val pop : pov -> 'a t -> 'a measure -> owner -> 'a * 'a t

  (** [get p i] returns the element found at index [i] in the sequence [p].
      [i] must be comprised between 0 included and [length p] excluded. *)
  val get : 'a t -> index -> 'a

  (** [concat p1 p2 o] concatenates the sequences [p1] and [p2], yielding a
      sequence. The schunks [p1] and [p2] must have the same capacity. The
      length of the concatenated sequence must not exceed this capacity: that
      is, [length p1 + length p2 <= capacity p1] must hold. *)
  val concat : 'a t -> 'a t -> owner -> 'a t

  (** [reach m p w] returns a pair of the total weight of the elements that
      precede [x] in the sequence [p] and the index in the sequence of the
      element [x] within which the weight [w] falls. The weight index [w] must
      be comprised between 0 included and [weight p] excluded. *)
  val reach : 'a measure -> 'a t -> weight -> weight * index

  (** [reach_from m p pindex pwindex w] is a generalization of [reach].
      Compared with [reach], it takes two extra arguments, an index
      [pindex] and a corresponding weight index [pwindex], which describe a
      starting position in the schunk [p]. ([pwindex] must be consistent with
      [pindex]: that is, it must be the sum of the weights of the elements
      whose index is less than [pindex].) The schunk [p] is scanned from the
      position [pindex]: it is scanned forward if [pwindex <= w] holds, and
      backwards if [w < pwindex] holds. *)
  val reach_from :
    'a measure -> 'a t -> index -> weight -> weight -> weight * index

  (** [get_by_weight m p w] finds the element [x] at weight-index [w] in the
      schunk [p]. It returns a pair of an adjusted weight-index (namely [w]
      minus the weight of the elements that precede [x]) and the element [x]. *)
  val get_by_weight : 'a measure -> 'a t -> weight -> weight * 'a

  (** [update_by_weight m o f p w] finds the element [x] at weight-index [w]
      in the schunk [p], adjusts the weight-index [w] by subtracting from it
      the weight of the elements that precede [x], then passes [x] and [w]
      to the update function [f], which produces a new element [x']. The
      element [x] is then replaced with [x'] in the schunk. The elements
      [x] and [x'] must have the same weight. *)
  val update_by_weight : 'a measure -> owner -> 'a update -> 'a t update

  (** [three_way_split p i m o] splits the sequence [p] into a sequence [p1],
      followed with an element [x], followed with a sequence [p2], such that
      the weight [i] falls within the element [x], that is, [weight p1 <= i <
      weight p1 + apply m x] holds. The weight [i] must be comprised between 0
      included and [weight p] excluded. *)
  val three_way_split :
    'a t -> weight -> 'a measure -> owner -> 'a t * 'a * 'a t

  (** [take] is a specialized version of [three_way_split]. Instead of
      returning a triple [p1, x, p2], it returns a pair [p1, x]. *)
  val take : 'a t -> weight -> 'a measure -> owner -> 'a t * 'a

  (** [drop] is a specialized version of [three_way_split]. Instead of
      returning a triple [p1, x, p2], it returns a pair [x, p2]. *)
  val drop : 'a t -> weight -> 'a measure -> owner -> 'a * 'a t

  (** [remaining_length pov i p] returns the number of elements found ahead
      of the element at index [i] (included). *)
  val remaining_length : pov -> index -> 'a t -> length

  (** [segment pov i k p] returns the description of a segment of at most [k]
      consecutive elements, starting from the one at index [i] and progressing
      in the direction [pov]. The argument [k] must be no greater than
      [remaining_length pov i p].

      For additional details, see the documentation of [segment] in the
      signature [ECHUNK] above. *)
  val segment : pov -> index -> length -> 'a t -> 'a segment

  (** [segment_max pov i s] is a shorthand for [segment pov i k s] where [k]
      takes its maximum permitted value, that is, [remaining_length pov i p]. *)
  val segment_max : pov -> index -> 'a t -> 'a segment

  (** [iter_segments pov p] returns a sequence of one or two array segments
      that store the data of the schunk [p]. *)
  val iter_segments : pov -> 'a t -> 'a segments

  (** [iteri_segments_front] iterates over the segments in a schunk, while
      maintaining the index (with respect to the schunk) of the head of the
      current segment. It is restricted to the [Front] direction. *)
  val iteri_segments_front : 'a t ->
    (index -> 'a array * index * length -> unit) -> unit

  (** [print] is a schunk printer, parameterized with an element
      printer. It is intended to be used only while debugging. *)
  val print : 'a measure -> ('a -> PPrint.document) -> 'a t -> PPrint.document

end

(* -------------------------------------------------------------------------- *)

(** The module [ShareableSequence] implements a shareable sequence, which is
    essentially a persistent sequence data structure built on top of shareable
    chunks. This is its signature. *)
module type SSEQ = sig

  type 'a schunk

  type 'a measure =
  | MUnit         : 'a measure
  | MSWeight : 'a schunk measure

  (** This is the structure of a shareable sequence. *)
  type 'a t =
    | Zero  of { default : 'a; }
    | One   of { default : 'a; x : 'a }
    | Short of { default : 'a; a : 'a array }
    | Level of {
        weight : weight;
        front : 'a schunk;
        middle : 'a schunk t;
        back : 'a schunk;
      }

  (** Many of the functions below take an owner [o] as an argument. This owner
      is passed down to the functions that operate on schunks; it determines
      whether existing schunks can be modified in place or new schunks must be
      allocated, and serves as the creator of these newly-allocated schunks. *)

  (* Some functions take a [depth] as a parameter. At the outermost level,
     the depth is zero; every time we enter a middle sequence, the depth
     is incremented by one. *)

  (** [create default] is an empty sequence. *)
  val create : 'a -> 'a t

  (** [make default n v o] creates a sequence that consists of [n] copies of the
      value [v]. This sequence has depth [0]. *)
  val make : 'a -> length -> 'a -> owner -> 'a t

  (** [init default n f o] creates a sequence that consists of the values
      produced by the calls [f 0], [f 1], ... [f (n-1)], in this order.
      This sequence has depth [0]. *)
  val init : 'a -> length -> (index -> 'a) -> owner -> 'a t

  (** [nonempty_level pov weight this middle that] constructs a nonempty
      sequence whose front (resp. back) is [this], whose middle is [middle],
      and whose back (resp. front) is [that]. [weight] must be the total weight
      of the elements in the sequence, and must be nonzero. *)
  val nonempty_level :
    pov -> weight -> 'a schunk -> 'a schunk t -> 'a schunk -> 'a t

  (* [create_middle default] is an empty sequence of schunks. It is a
     short-hand for invoking [create] with a dummy schunk. *)
  val create_middle : 'a -> 'a schunk t

  (** [default s] returns a default element. (This element was provided as
      a default element in one of the operations that contributed to the
      construction of [s].) *)
  val default : 'a t -> 'a

  (** [weight s] is the total weight of the elements of the sequence [s]. *)
  val weight : 'a t -> weight

  (** [is_empty s] determines whether the sequence [s] is empty. *)
  val is_empty : 'a t -> bool

  (** [dummy s] extracts a dummy schunk out of [s], if possible; otherwise,
      it creates a fresh one. *)
  val dummy : 'a t -> 'a schunk

  (** [check s m o depth] verifies that the sequence [s] is valid with respect
      to measure [m], owner [o], and depth [depth]. *)
  val check : 'a t -> 'a measure -> owner -> depth -> unit

  (** [check_middle middle m o depth] verifies that the sequence [middle] is
      valid with respect to measure [m], owner [o], and depth [depth]. This
      includes checking that it is a valid sequence, checking that its elements
      are valid schunks, and verifing that the density invariant holds. *)
  val check_middle : 'a schunk t -> 'a measure -> owner -> depth -> unit

  (** [push pov s x m o depth] constructs and returns a new sequence obtained by
      pushing the element [x] onto the front or back end of the sequence [s]. *)
  val push : pov -> 'a t -> 'a -> 'a measure -> owner -> depth -> 'a t

  (** If the sequence [s] is nonempty, then [pop pov s m o] returns a pair of
      the element [x] found at the front or back end of the sequence [s] and
      of the sequence [s] deprived of [x]. If the sequence [s] is empty,
      the exception [Empty] is raised. *)
  val pop : pov -> 'a t -> 'a measure -> owner -> 'a * 'a t

  (** If the sequence [s] is nonempty, then [peek pov s] reads the element
      [x] found at the front or back end of the sequence [s] and returns [x].
      If the sequence [s] is empty, the exception [Empty] is raised. *)
  val peek : pov -> 'a t -> 'a

  (** [get s i m] is equivalent to
      [let s1, x, s2 = three_way_split s i m o in (i - weight s1, x)].
      Thus, it returns a pair of the element [x] within which the weight [i]
      falls and the index [i] minus the weight of the elements located towards
      the left of [x]. *)
  val get : 'a t -> weight -> 'a measure -> weight * 'a

  (** [set s i m o x] returns a new sequence obtained by replacing the element
      within which the weight [i] falls with the new element [x]. The old and
      new element must have the same weight. *)
  val set : 'a t -> weight -> 'a measure -> owner -> 'a -> 'a t

  (** [update m o f s i] applies the update [f] at weight-index [i] in the
      sequence [s]. The update function [f] is itself parameterized with the
      weight-index at which the update should be applied. *)
  val update : 'a measure -> owner -> 'a update -> 'a t update

  (** [fuse_back middle p o depth] pushes the schunk [p] onto the back of
      [middle], which is a sequence of schunks. If possible, the last schunk of
      the sequence and the schunk [p] are fused into a single schunk. By
      convention, pass [depth], not [depth + 1] -- [fuse_back] takes care of
      adding one. *)
  val fuse_back : 'a schunk t -> 'a schunk -> owner -> depth -> 'a schunk t

  (** [fuse s1 s2 o depth] concatenates the sequences [s1] and [s2], which are
      sequences of schunks. It attempts to fuse the last schunk of [s1] and the
      first schunk of [s2], so as to preserve the fill invariant. By
      convention, pass [depth], not [depth + 1] -- [fuse] takes care of adding
      one. *)
  val fuse : 'a schunk t -> 'a schunk t -> owner -> depth -> 'a schunk t

  (** [concat s1 s2 o depth] concatenates the sequences [s1] and [s2]. *)
  val concat : 'a t -> 'a t -> owner -> depth -> 'a t

  (** [three_way_split s i m o] splits the sequence [s] into a sequence
      [s1], followed with an element [x], followed with a sequence [s2], such
      that the weight [i] falls within the element [x], that is, [weight s1 <=
      i < weight s1 + apply m x] holds. The weight [i] must be comprised
      between 0 included and [weight s] excluded. *)
  val three_way_split :
    'a t -> weight -> 'a measure -> owner -> 'a t * 'a * 'a t

  (** [take] is a specialized version of [three_way_split]. Instead of
      returning a triple [s1, x, s2], it returns a pair [s1, x]. *)
  val take : 'a t -> weight -> 'a measure -> owner -> 'a t * 'a

  (** [drop] is a specialized version of [three_way_split]. Instead of
      returning a triple [s1, x, s2], it returns a pair [x, s2]. *)
  val drop : 'a t -> weight -> 'a measure -> owner -> 'a * 'a t

  (** [to_array s] returns a fresh array whose elements are the elements
      of the sequence [s]. *)
  val to_array : 'a t -> 'a array

  (** [of_array_segment default a head size o] creates a sequence out of the
      array segment defined by the array [a], the start index [head], and the
      size [size]. This function only makes sense at depth [0]. *)
  val of_array_segment : 'a -> 'a array -> index -> length -> owner -> 'a t

  (** [iter pov f s] applies the function [f] in turn to every element in
      the sequence [s], in the order specified by the point-of-view [pov]. *)
  val iter : pov -> ('a -> unit) -> 'a t -> unit

  (** [iter_segments pov s] returns a sequence of array segments that store
      the data of the sequence [s]. *)
  val iter_segments : pov -> 'a t -> 'a segments

  (** [print] is a sequence printer, parameterized with an element printer.
      It is intended to be used only while debugging. *)
  val print : 'a measure -> ('a -> PPrint.document) -> 'a t -> PPrint.document

end

(* -------------------------------------------------------------------------- *)

(** The signature {!WITER} is our internal API for iterators on weighted
    sequences. *)
module type WITER = sig
  type 'a measure
  (* Elements that also appear in the signature [IITER],
     albeit with more complex types here: *)
  type 'a t
  type 'a iter
  val create : pov -> 'a t -> 'a measure -> 'a iter
  val reset : pov -> 'a iter -> 'a measure -> unit
  val copy : 'a iter -> 'a iter
  val sequence: 'a iter -> 'a t
  val weight : 'a iter -> weight
  val windex : 'a iter -> 'a measure -> weight
  val finished : 'a iter -> 'a measure -> bool
  val get : 'a iter -> 'a measure -> 'a
  val set : 'a iter -> 'a measure -> 'a -> unit
  val move : pov -> 'a iter -> 'a measure -> unit
  val jump : pov -> 'a iter -> int -> 'a measure -> unit
  val get_segment : pov -> 'a iter -> 'a measure -> 'a segment
  val get_writable_segment : pov -> 'a iter -> 'a measure -> 'a segment
  val reach : 'a iter -> weight -> 'a measure -> unit
  val check: 'a iter -> 'a measure -> unit
  val print: ('a -> PPrint.document) -> 'a iter -> 'a measure -> PPrint.document
  (* Elements that appear only here: *)
  val reach_inside : 'a iter -> weight -> 'a measure -> unit
  val create_at_sentinel : pov -> 'a t -> 'a measure -> 'a iter
  val is_at_weight : 'a iter -> 'a measure -> weight -> bool
  val unchecked_get : 'a iter -> 'a measure -> 'a
  val is_valid : 'a iter -> bool
end

(* -------------------------------------------------------------------------- *)

(** The signature {!IITER} is our internal iterator API. It is essentially
    a subset of the public signature [ITER]. *)
module type IITER = sig
  type 'a t
  type 'a iter
  val create : pov -> 'a t -> 'a iter
  val reset : pov -> 'a iter -> unit
  val copy : 'a iter -> 'a iter
  val sequence : 'a iter -> 'a t
  val length : 'a iter -> length
  val is_valid : 'a iter -> bool
  val index : 'a iter -> index
  val finished : 'a iter -> bool
  val get : 'a iter -> 'a
  val move : pov -> 'a iter -> unit
  val jump : pov -> 'a iter -> int -> unit
  val get_segment : pov -> 'a iter -> 'a segment
  val get_writable_segment : pov -> 'a iter -> 'a segment
  val reach : 'a iter -> index -> unit
  val set : 'a iter -> 'a -> unit
  val check : 'a iter -> unit
  val print : ('a -> PPrint.document) -> 'a iter -> PPrint.document
end

(* -------------------------------------------------------------------------- *)

(** The module [PersistentSequence] offers an implementation of persistent
    sequences: it has type [PSEQ]. The functor [ShortPersistentSequence.Make]
    accepts such an implementation and produces another implementation, where
    short sequences have a simple and compact representation: it has type
    [PSEQ -> PSEQ]. *)

(** The signature [PSEQ] is essentially a subset of the public signature of
    persistent sequences in [PublicSignatures]. For this reason, we do not
    comment it. *)

module type PSEQ = sig
  (** Revealing the definition of this type allows the code in the module
      ShortPersistentSequence to work on this data structure directly,
      without introducing an extra layer of indirection. *)
  type 'a schunk
  type 'a t =
    | Zero  of { default : 'a; }
    | One   of { default : 'a; x : 'a }
    | Short of { default : 'a; a : 'a array }
    | Level of {
        weight : weight;
        front : 'a schunk;
        middle : 'a schunk t;
        back : 'a schunk;
      }
  val create : 'a -> 'a t
  val make : 'a -> length -> 'a -> 'a t
  val init : 'a -> length -> (index -> 'a) -> 'a t
  val default : 'a t -> 'a
  val length : 'a t -> length
  val is_empty : 'a t -> bool
  val push : pov -> 'a t -> 'a -> 'a t
  val pop : pov -> 'a t -> 'a * 'a t
  val peek : pov -> 'a t -> 'a
  val get : 'a t -> index -> 'a
  val set : 'a t -> index -> 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val split : 'a t -> index -> 'a t * 'a t
  val take : 'a t -> index -> 'a t
  val drop : 'a t -> index -> 'a t
  val sub : 'a t -> index -> length -> 'a t
  val iter_segments : pov -> 'a t -> 'a segments
  val to_array : 'a t -> 'a array
  val of_array_segment : 'a -> 'a array -> index -> length -> 'a t
  val of_array : 'a -> 'a array -> 'a t
  val print : ('a -> PPrint.document) -> 'a t -> PPrint.document
  val check : 'a t -> unit
end
