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

(** @inline *)
include PublicTypeAbbreviations
  (* [include] instead of [open] leads to better documentation *)

(** {1 Library API} *)

(** The signature {!SEK} is the public API of the library. If you are a new
    user, you do {i not} need to follow this link: the library's API appears
    below anyway. Just read on! *)

module type SEK = sig

  type side
  val front : side
  val back  : side
  (** A side appears as a parameter to several operations, such as [push]
      and [pop], which can operate at either end of a sequence. *)

  type direction
  val forward  : direction
  val backward : direction
  (** A direction appears as a parameter to several operations, such as
      [iter], which can traverse the sequence either forward (from front
      to back) or backward (from back to front). *)

  (** The exception [Empty] is raised by [pop] and [peek] operations when
      applied to an empty sequence. *)
  exception Empty

  (** The submodule {!Ephemeral}, also available under the name [E],
      offers an implementation of ephemeral (mutable) sequences. *)
  module Ephemeral : sig

    (** A sequence [s] of type ['a t] is a mutable data structure which
        represents a mathematical sequence of elements of type ['a]. *)
    type 'a t

    (** [create default] constructs and returns a new empty sequence. The
        default value [default] is used to overwrite array slots that become
        empty during operations such as [clear], [pop], or [split]. *)
    val create : 'a -> 'a t

    (** [make default n v] constructs and returns a fresh sequence whose
        length is [n] and which consists of [n] copies of the value [v].
        It is equivalent to [of_array default (Array.make n v)]. *)
    val make : 'a -> length -> 'a -> 'a t

    (** [init default n f] constructs and returns a fresh sequence whose
        length is [n] and whose elements are the values produced by the
        calls [f 0], [f 1], ... [f (n-1)], in this order. It is equivalent
        to [of_array default (Array.init n f)]. *)
    val init : 'a -> length -> (index -> 'a) -> 'a t

    (** [default s] returns a value that was supplied as an argument in a
        call to [create] that contributed to the construction of the
        sequence [s]. *)
    val default : 'a t -> 'a

    (** [length s] returns the length of the sequence [s]. *)
    val length : 'a t -> length

    (** [is_empty s] returns [true] if the sequence [s] is empty and [false]
        otherwise. It is equivalent to [length s = 0]. *)
    val is_empty : 'a t -> bool

    (** [clear s] empties the sequence [s]. *)
    val clear : 'a t -> unit

    (** [copy s] constructs and returns a copy [s'] of the sequence [s]. The
        sequences [s] and [s'] initially have the same elements, and can
        thereafter be modified independently of one another. Although [copy]
        itself is very fast, it does have an indirect cost. As a result of the
        copy, the sequences [s] and [s'] lose the unique ownership of their
        chunks; this causes additional copies to take place during subsequent
        update operations on either [s] or [s']. *)
    val copy : 'a t -> 'a t

    (** If [s1] and [s2] are distinct sequences, then [assign s1 s2] moves
        [s2]'s elements into [s1], overwriting [s1]'s previous content, and
        clears [s2]. If [s1] and [s2] are the same sequence, then [assign s1
        s2] has no effect. *)
    val assign: 'a t -> 'a t -> unit

    (** [push side s x] pushes the element [x] onto the front or back end
        of the sequence [s]. The parameter [side] determines which end of
        the sequence is acted upon. *)
    val push: side -> 'a t -> 'a -> unit

    (** If the sequence [s] is nonempty, then [pop side s] pops an element [x]
        off the front or back end of the sequence [s] and returns [x]. The
        parameter [side] determines which end of the sequence is acted upon. If
        the sequence [s] is empty, the exception {!Empty} is raised. *)
    val pop : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [pop_opt side s] pops an element
        [x] off the front or back end of the sequence [s] and returns [Some x].
        The parameter [side] determines which end of the sequence is acted
        upon. If the sequence [s] is empty, [None] is returned. *)
    val pop_opt : side -> 'a t -> 'a option

    (** If the sequence [s] is nonempty, then [peek side s] reads the element
        [x] found at the front or back end of the sequence [s] and returns [x].
        The parameter [side] determines which end of the sequence is acted
        upon. If the sequence [s] is empty, the exception {!Empty} is
        raised. *)
    val peek : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [peek_opt side s] reads the
        element [x] found at the front or back end of the sequence [s] and
        returns [Some x]. The parameter [side] determines which end of the
        sequence is acted upon. If the sequence [s] is empty, [None] is
        returned. *)
    val peek_opt : side -> 'a t -> 'a option

    (** [get s i] returns the element [x] located at index [i] in the sequence
        [s]. The index [i] must be comprised between 0 included and [length s]
        excluded. *)
    val get: 'a t -> index -> 'a

    (** [set s i x] replaces the element located at index [i] in the sequence
        [s] with the element [x]. The index [i] must be comprised between 0
        included and [length s] excluded. The sequence [s] is updated in
        place. *)
    val set : 'a t -> index -> 'a -> unit

    (** [concat s1 s2] creates and returns a new sequence whose content is the
        concatenation of the sequences [s1] and [s2]. The sequences [s1] and
        [s2] are cleared. The sequences [s1] and [s2] must be distinct.
        [concat] is slightly less efficient than [append], whose use should be
        preferred. *)
    val concat: 'a t -> 'a t -> 'a t

    (** [append back s1 s2] is equivalent to [assign s1 (concat s1 s2)]. Thus,
        [s1] is assigned the concatenation of the sequences [s1] and [s2],
        while [s2] is cleared. In other words, [append back s1 s2] appends
        the sequence [s2] at the back end of the sequence [s1].

        [append front s1 s2] is equivalent to [assign s1 (concat s2 s1)]. Thus,
        [s1] is assigned the concatenation of the sequences [s2] and [s1],
        while [s2] is cleared. In other words, [append front s1 s2] prepends
        the sequence [s2] at the front end of the sequence [s1].

        In either case, the sequences [s1] and [s2] must be distinct. *)
    val append : side -> 'a t -> 'a t -> unit

    (** [split s i] splits the sequence [s] at index [i]. It returns two new
        sequences [s1] and [s2] such that the length of [s1] is [i] and the
        concatenation of [s1] and [s2] is [s]. The sequence [s] is cleared. The
        index [i] must be comprised between 0 and [length s], both included.
        [split] is slightly less efficient than [carve], whose use should be
        preferred. *)
    val split : 'a t -> index -> 'a t * 'a t

    (** [carve back s i] is equivalent to
        [let s1, s2 = split s i in assign s s1; s2].
        Thus, it splits the sequence [s] at index [i] into two parts: the
        first part is written to [s], while the second part is returned.

        [carve front s i] is equivalent to
        [let s1, s2 = split s i in assign s s2; s1].
        Thus, it splits the sequence [s] at index [i] into two parts: the
        second part is written to [s], while the first part is returned.

        In either case, the index [i] must be comprised between 0 and
        [length s], both included. *)
    val carve : side -> 'a t -> index -> 'a t

    (** [iter direction f s] applies the function [f] in turn to every element
        [x] of the sequence [s]. The parameter [direction] determines in what
        order the elements are presented. The function [f] is not allowed to
        modify the sequence [s] while iteration is ongoing. *)
    val iter : direction -> ('a -> unit) -> 'a t -> unit

    (** [iteri direction f s] applies the function [f] in turn to every index
        [i] and matching element [x] of the sequence [s]. The parameter
        [direction] determines in what order the elements are presented. The
        function [f] is not allowed to modify the sequence [s] while iteration
        is ongoing. *)
    val iteri : direction -> (index -> 'a -> unit) -> 'a t -> unit

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_left f a s] is equivalent to [List.fold_left f a (to_list s)]. *)
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the backward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_right f s a] is equivalent to [List.fold_right f (to_list s) a]. *)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** [to_list s] returns a list whose elements are the elements of the
        sequence [s]. *)
    val to_list : 'a t -> 'a list

    (** [to_array s] returns a fresh array whose elements are the elements
        of the sequence [s]. *)
    val to_array : 'a t -> 'a array

    (** [of_array_segment default a head size] creates a new sequence out of
        the array segment defined by the array [a], the start index [head], and
        the size [size]. The data is copied, so the array [a] can still be used
        afterwards. *)
    val of_array_segment : 'a -> 'a array -> index -> length -> 'a t

    (** [of_array default a] creates a new sequence out of the array [a]. The
        data is copied, so the array [a] can still be used afterwards. *)
    val of_array : 'a -> 'a array -> 'a t

    (** [format] is a printer for sequences of integers. It can be installed
        in the OCaml toplevel loop by [#install_printer format]. It is intended
        to be used only while debugging the library. *)
    val format: Format.formatter -> int t -> unit

    (** In a release build, this function does nothing. In a development build,
        it checks that the data structure's internal invariant is satisfied. *)
    val check : 'a t -> unit

  end (* Ephemeral *)

  (** The submodule {!Persistent}, also available under the name [P],
      offers an implementation of persistent (immutable) sequences. *)
  module Persistent : sig

    (** A sequence [s] of type ['a t] is an immutable data structure which
        represents a mathematical sequence of elements of type ['a]. *)
    type 'a t

    (** [create default] constructs and returns a new empty sequence. The
        default value [default] is used to overwrite array slots that become
        empty during operations such as [clear], [pop], or [split]. *)
    val create : 'a -> 'a t

    (** [make default n v] constructs and returns a fresh sequence whose
        length is [n] and which consists of [n] copies of the value [v].
        It is equivalent to [of_array default (Array.make n v)]. *)
    val make : 'a -> length -> 'a -> 'a t

    (** [init default n f] constructs and returns a fresh sequence whose
        length is [n] and whose elements are the values produced by the
        calls [f 0], [f 1], ... [f (n-1)], in this order. It is equivalent
        to [of_array default (Array.init n f)]. *)
    val init : 'a -> length -> (index -> 'a) -> 'a t

    (** [default s] returns a value that was supplied as an argument in a
        call to [create] that contributed to the construction of the
        sequence [s]. *)
    val default : 'a t -> 'a

    (** [length s] returns the length of the sequence [s]. *)
    val length : 'a t -> length

    (** [is_empty s] returns [true] if the sequence [s] is empty and [false]
        otherwise. It is equivalent to [length s = 0]. *)
    val is_empty : 'a t -> bool

    (** [push side s x] constructs and returns a new sequence obtained by
        pushing the element [x] onto the front or back end of the sequence [s].
        The parameter [side] determines which end of the sequence is acted
        upon. *)
    val push : side -> 'a t -> 'a -> 'a t

    (** If the sequence [s] is nonempty, then [pop side s] returns a pair of
        the element [x] found at the front or back end of the sequence [s] and
        of the sequence [s] deprived of [x]. The parameter [side] determines
        which end of the sequence is acted upon. If the sequence [s] is empty,
        the exception {!Empty} is raised. *)
    val pop : side -> 'a t -> 'a * 'a t

    (** If the sequence [s] is nonempty, then [pop_opt side s] returns a pair
        [(Some x, s')] where [x] is the element found at the front or back end
        of the sequence [s] and [s'] is the sequence [s] deprived of [x]. The
        parameter [side] determines which end of the sequence is acted upon. If
        the sequence [s] is empty, the pair [(None, s)] is returned. *)
    val pop_opt : side -> 'a t -> 'a option * 'a t

    (** If the sequence [s] is nonempty, then [peek side s] reads the element
        [x] found at the front or back end of the sequence [s] and returns [x]. The
        parameter [side] determines which end of the sequence is acted upon.
        If the sequence [s] is empty, the exception {!Empty} is raised. *)
    val peek : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [peek_opt side s] reads the
        element [x] found at the front or back end of the sequence [s] and
        returns [Some x]. The parameter [side] determines which end of the
        sequence is acted upon. If the sequence [s] is empty, [None] is
        returned. *)
    val peek_opt : side -> 'a t -> 'a option

    (** [get s i] returns the element [x] located at index [i] in the sequence
        [s]. The index [i] must be comprised between 0 included and [length s]
        excluded. *)
    val get: 'a t -> index -> 'a

    (** [set s i x] returns a new sequence obtained by replacing the element
        located at index [i] in the sequence [s] with the element [x]. The
        index [i] must be comprised between 0 included and [length s] excluded.
        The sequence [s] is not affected. *)
    val set : 'a t -> index -> 'a -> 'a t

    (** [concat s1 s2] returns a new sequence obtained by
        concatenating the sequences [s1] and [s2]. *)
    val concat : 'a t -> 'a t -> 'a t

    (** [split s i] splits the sequence [s] at index [i]. It returns two
        sequences [s1] and [s2] such that the length of [s1] is [i] and the
        concatenation of [s1] and [s2] is [s]. The index [i] must be comprised
        between 0 and [length s], both included. *)
    val split : 'a t -> index -> 'a t * 'a t

    (** [iter direction f s] applies the function [f] in turn to every element
        [x] of the sequence [s]. The parameter [direction] determines in what
        order the elements are presented. The function [f] is not allowed to
        modify the sequence [s] while iteration is ongoing. *)
    val iter : direction -> ('a -> unit) -> 'a t -> unit

    (** [iteri direction f s] applies the function [f] in turn to every index
        [i] and matching element [x] of the sequence [s]. The parameter
        [direction] determines in what order the elements are presented. The
        function [f] is not allowed to modify the sequence [s] while iteration
        is ongoing. *)
    val iteri : direction -> (index -> 'a -> unit) -> 'a t -> unit

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_left f a s] is equivalent to [List.fold_left f a (to_list s)]. *)
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the backward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_right f s a] is equivalent to [List.fold_right f (to_list s) a]. *)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** [to_list s] returns a list whose elements are the elements of the
        sequence [s]. *)
    val to_list : 'a t -> 'a list

    (** [to_array s] returns a fresh array whose elements are the elements
        of the sequence [s]. *)
    val to_array : 'a t -> 'a array

    (** [of_array_segment default a head size] creates a new sequence out of
        the array segment defined by the array [a], the start index [head], and
        the size [size]. The data is copied, so the array [a] can still be used
        afterwards. *)
    val of_array_segment : 'a -> 'a array -> index -> length -> 'a t

    (** [of_array default a] creates a new sequence out of the array [a]. The
        data is copied, so the array [a] can still be used afterwards. *)
    val of_array : 'a -> 'a array -> 'a t

    (** [format] is a printer for sequences of integers. It can be installed
        in the OCaml toplevel loop by [#install_printer format]. It is intended
        to be used only while debugging the library. *)
    val format: Format.formatter -> int t -> unit

    (** In a release build, this function does nothing. In a development build,
        it checks that the data structure's internal invariant is satisfied. *)
    val check : 'a t -> unit

  end (* Persistent *)

  (** [E] is a short name for the submodule {!Ephemeral}. *)
  module E = Ephemeral

  (** [P] is a short name for the submodule {!Persistent}. *)
  module P = Persistent

  (** {2 Conversion Functions} *)

  (** The following functions offer fast conversions between ephemeral and
      persistent sequences. *)

  (** [snapshot s] constructs and returns a persistent sequence whose elements
      are the elements of [s]. It is less efficient than [snapshot_and_clear],
      whose use should be preferred, when possible. *)
  val snapshot : 'a Ephemeral.t -> 'a Persistent.t

  (** [snapshot_and_clear s] constructs and returns a persistent sequence
      whose elements are the elements of [s]. As a side effect, it clears
      [s]. *)
  val snapshot_and_clear : 'a Ephemeral.t -> 'a Persistent.t

  (** [edit s] constructs and returns a new ephemeral sequence whose elements
      are the elements of [s]. *)
  val edit : 'a Persistent.t -> 'a Ephemeral.t

  (** {2 Emulation Layers} *)

  (** The submodule {!Queue} is a replacement for OCaml's standard [Queue]
      module, where a queue is implemented as an ephemeral sequence. Elements
      are enqueued at the back end of the sequence and dequeued at the front
      end. *)
  module Queue : sig
    type 'a t = 'a E.t
    exception Empty
    val create : 'a -> 'a t
    val add : 'a -> 'a t -> unit
    val push : 'a -> 'a t -> unit
    val take : 'a t -> 'a
    val take_opt : 'a t -> 'a option
    val pop : 'a t -> 'a
    val peek : 'a t -> 'a
    val peek_opt : 'a t -> 'a option
    val top : 'a t -> 'a
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> depth
    val iter : ('a -> unit) -> 'a t -> unit
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val transfer : 'a t -> 'a t -> unit
  end

  (** The submodule {!Stack} is a replacement for OCaml's standard [Stack]
      module, where a stack is implemented as an ephemeral sequence. Elements
      are pushed and popped at the front end of the sequence. *)
  module Stack : sig
    type 'a t = 'a E.t
    exception Empty

    (** [create] requires a default value. The functor [SupplyDefault]
        remedies this problem. *)
    val create : 'a -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val pop_opt : 'a t -> 'a option
    val top : 'a t -> 'a
    val top_opt : 'a t -> 'a option
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val iter : ('a -> unit) -> 'a t -> unit
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  end

  (** {2 Miscellaneous} *)

  (** The function call [released()] does nothing if the library was
      compiled in release mode, and fails (with an assertion failure)
      if the library was compiled with assertions enabled. *)
  val released: unit -> unit

end
