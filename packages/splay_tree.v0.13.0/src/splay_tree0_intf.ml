open Core_kernel

(** Splay trees are binary search trees that move recently accessed nodes closer to the
    root for easier access.  They have amortized O(log n)-time access for a large enough
    sequence of primitive operations.

    As a heuristic, a splay tree may outperform other trees such as red-black trees when
    recently accessed items are more likely to be accessed in the near future.

    The amortized complexity analysis only applies if [t] is used as a linear type, i.e.
    each [t] returned by access operations is used for the next operation, instead of
    being discarded (similar to Fqueue). If, instead, it is used as a persistent data
    structure, most primitive operations have O(n) complexity.
*)

module type Key = sig
  type t [@@deriving sexp, compare]
end

module type Data = sig
  type t [@@deriving sexp]
end

module type Reduction_operation = sig
  type key
  type data
  type accum

  val identity : accum
  val singleton : key:key -> data:data -> accum

  (** [combine] is required to be associative and have [identity] as its identity.
      In other words, they must form a monoid. *)
  val combine : accum -> accum -> accum
end

type ('k, 'd, 'a) reduction_operation =
  (module Reduction_operation with type key = 'k and type data = 'd and type accum = 'a)

module type S = sig
  type t [@@deriving sexp]
  type key [@@deriving sexp]
  type data [@@deriving sexp]
  type accum

  val empty : t
  val of_alist : (key * data) list -> t Or_error.t
  val of_alist_exn : (key * data) list -> t
  val to_alist : t -> (key * data) list
  val is_empty : t -> bool
  val length : t -> int
  val accum : t -> accum
  val keys : t -> key list
  val data : t -> data list
  val mem : t -> key -> bool
  val find : t -> key -> data option
  val set : t -> key:key -> data:data -> t
  val remove : t -> key -> t


  val remove_min : t -> (key * data * t) option
  val remove_max : t -> (key * data * t) option
  val remove_after : t -> key -> (key * data * t) option
  val remove_before : t -> key -> (key * data * t) option
  val map : t -> f:(data -> data) -> t

  val map_range
    :  t
    -> min_key:key
    -> max_key:key
    -> f:((key * data) list -> (key * data) list)
    -> t

  val nth : t -> int -> (key * data) option

  (** [rank t key] is the number of nodes before where [key] would be inserted. In other
      words, the length of the left subtree after a [split t key]. *)
  val rank : t -> key -> int

  (** [search] implements bisection search over [t] based on the [accum] values
      of its prefixes.

      Let's consider a [t] consisting of four elements [a; b; c; d] (see diagram below)
      (we'll refer to all of key, data, and accum of [a] as just [a]; we'll also assume
      [R.combine = (+)]).

      Then there are five possible positions to evaluate [f] at (numbered 0..4 below):

      +
      | position:       0     1     2     3     4
      |                 -------------------------
      | element:        |  a  |  b  |  c  |  d  |
      |                 -------------------------
      | left:           0     a    a+b  a+b+c a+b+c+d
      | right:      a+b+c+d b+c+d  c+d    d     0
      | f ~left ~right: R     R     R     L     L
      +

      The function [f ~left ~right] will be called for a particular position and it takes
      the sum of the elements to the left and to the right of this position.
      This means [left + right] will be equal to [accum t].

      The return value of [f] must indicate the direction where the desired element is.
      In the diagram above [f ~left:(a+b) ~right:(c+d)] returns [`Right],
      whereas [f ~left:(a+b+c) ~right:d] returns [`Left], which makes [c] the desired
      element.

      Therefore, [search] will return [c]. If [f] returns the same value at all positions,
      [search] returns [None].

      For it to make sense, [f] must be monotonic: calling [f] on every possible position
      from left to right should produce a prefix of [`Right] values followed by a
      suffix of [`Left] values.

      Example:

      If the values are positive integers and reduction operation is sum, then you can
      find the last node where the prefix sum before it is at most [x] with the
      following [f]:
      {[
        let f ~left ~right:_ = if x < left then `Left else `Right
      ]}
  *)
  val search
    :  t
    -> f:(left:accum -> right:accum -> [ `Right | `Left ])
    -> (key * data) option

  module Partition : sig
    type nonrec t =
      { (* [lt] = values less than the [min_key] of the partition *)
        lt : t
      ; (* [mid] = values between [min_key] and [max_key] *)
        mid : t
      ; (* [gt] = values greater than the [max_key] of the partition *)
        gt : t
      }
  end

  val partition : ?min_key:key -> ?max_key:key -> t -> Partition.t

  (** [subrange t ?min_key ?max_key] is equivalent to the [mid] in [partition] *)
  val subrange : ?min_key:key -> ?max_key:key -> t -> t

  val merge
    :  t
    -> t
    -> f:(key:key
          -> [ `Left of data | `Right of data | `Both of data * data ]
          -> data option)
    -> t

  val split : t -> key -> t * data option * t

  (** [join t1 t2] directly concatenates the sequences described by [t1] and [t2]. This
      should be used to rejoin trees obtained by a split operation, though it can be used
      for other things.

      This operation can fail if the concatenation of the two sequences is invalid; i.e.
      the keys are not in order. This happens when the maximum key of [t1] is at or above
      the minimum key of [t2].

      Currently the cost of [join] is not fully amortized, so it should be considered
      worst-case linear time.
  *)
  val join : t -> t -> t Or_error.t

  val join_exn : t -> t -> t
end

module type Splay_tree = sig
  module type Key = Key
  module type Data = Data
  module type Reduction_operation = Reduction_operation

  type nonrec ('k, 'd, 'a) reduction_operation = ('k, 'd, 'a) reduction_operation

  module type S = S

  module Make_with_reduction
      (Key : Key)
      (Data : Data)
      (R : Reduction_operation with type key = Key.t and type data = Data.t) :
    S with type key = Key.t and type data = Data.t and type accum = R.accum

  module Make_without_reduction (Key : Key) (Data : Data) :
    S with type key = Key.t and type data = Data.t and type accum = unit

  module Reduction_operations : sig
    val reduce2
      :  ('k, 'd, 'a) reduction_operation
      -> ('k, 'd, 'b) reduction_operation
      -> ('k, 'd, 'a * 'b) reduction_operation
  end
end

