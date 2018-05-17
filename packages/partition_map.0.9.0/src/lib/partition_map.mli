(** Interval's represent consecutive regions of the positive integer domain.

    This interface is purposefully undocumented as it is not meant to be used
    without understanding the implementation. At the moment, we still need:
      - [Ascending.fold_set_and_values]
      - [Set.iter]
      - [Set.size]
    to be exposed to for efficient operations in Prohlatype and a proper
    refactor is beyond the scope of current work.
*)
module Interval : sig

  type t

  val compare : t -> t -> int

  val max_value : int

  val make : start:int -> end_:int -> t

  val extend_one : t -> t

  val width : t -> int

  val inside : int -> t -> bool

  val start : t -> int

  val end_ : t -> int

  val to_string : t -> string

  val is_none : t -> bool

  val strict_before : t -> t -> bool

  val before_separate : t -> t -> bool

  val merge : t -> t -> t

  val split_inter_diff2 : t -> t ->
                          t * t * t * t * t
  val split_inter_diff3 : t -> t -> t ->
                          t * t * t * t * t * t * t
  val split_inter_diff4 : t -> t -> t -> t ->
                          t * t * t * t * t * t * t * t * t

  val aligned_inter_diff2 : t -> t ->
                            t * t * t
  val aligned_inter_diff3 : t -> t -> t ->
                            t * t * t * t
  val aligned_inter_diff4 : t -> t -> t -> t ->
                            t * t * t * t * t

  val fold : t -> init:'a -> f:('a -> int -> 'a) -> 'a

  val iter : t -> f:(int -> unit) -> unit

  val cpair : int -> t -> t -> t list

  val to_cross_indices : int -> t -> (int * int) list

end (* Interval *)

(** Sets of [Interval]s.

  This interface is purposefully undocumented as it is not meant to be used
  without understanding the implementation.
*)
module Set : sig

  type t = Interval.t list

  val to_string : t -> string

  val empty : t

  val is_empty : t -> bool

  (* Number of elements in the set. *)
  val size : t -> int

  (* Number of intervals, a faster proxy for size. *)
  val length : t -> int

  val inside : int -> t -> bool

  (* [intersection_and_differences t1 t2] returns the intersection, and the
     differences; remaining in [t1] and remaining [t2], respectively. *)
  val intersection_and_differences : t -> t -> t * t * t

  val iter : t -> f:(int -> unit) -> unit

end (* Set *)

(** A partition map is a data structure that tracks states using partitions
    of the domain elements.

  Specifically, if we know (and can enumerate) the elements of a set this data
  structure allows a mapping from elements to the values.

  Internally, it maintains partitions: representations of sets of the elements
  that partitions the entire universe. The most important operation is the
  {merge} of 2 (such as [merge4]) such partition maps.
*)

(** We frequently need to test the equality of values stored Partition Maps,
    consequently many methods take such a predicate as an argument.

    As of writing (2018-05-16) A Functorized approach compiled with
    4.06.0+Flambda is still marginally slower than passing around this equality
    predicate. *)
type 'a equality = 'a -> 'a -> bool

(** It is recommended that one construct partition map's in [Descending] order
    and then convert them into the ascending order for merging
    [Ascending.of_descending]. *)
module Descending : sig

  type +'a t

  (* Empty constructors. *)
  val empty : 'a t

  (* Test whether a descending partition map is empty. *)
  val is_empty : 'a t -> bool

  (* Initializers. Create a partition map keyed off of just the first element
     in the domain. *)
  val singleton : 'a -> 'a t

  (* Size of the domain. *)
  val size : 'a t -> int

  (* Convert to string. *)
  val to_string : 'a t -> ('a -> string) -> string

  (* Observe a value for the next element. *)
  val add : eq:'a equality -> 'a -> 'a t -> 'a t

end (* Descending *)

module Ascending : sig

  type +'a t

  (** Empty constructor. *)
  val empty : 'a t

  (** Test whether a ascending partition map is empty. *)
  val is_empty : 'a t -> bool

  (** Convert a descending partition map into an ascending. *)
  val of_descending : eq:'a equality -> 'a Descending.t -> 'a t

  (** [of_ascending_interval_list] converts ascending pairs of intervals
     into an ascending partition map.
     For example: [(0,100),'a'; (101,200), 'b'].

     @raise {Invalid_argument} If the first value of the pair in the first
      position is not 0 or the intervals are not ascending, or the values
      between the end and successive start are not adjacent
      (eg. next start = previous end + 1). *)
  val of_ascending_interval_list : eq:'a equality
                                 -> ((int * int) * 'a) list
                                 -> 'a t

  (** Initialize a partition map of the given size with one value. *)
  val init : size:int -> 'a -> 'a t

  (** Convert to a string. *)
  val to_string : 'a t -> ('a -> string) -> string

  (** Test for equality. *)
  val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** [get t i] returns the value associated  with the [i]'th domain element.

      @raise {Not_found} if [i] is outside the range [0, (size t)). *)
  val get : 'a t -> int -> 'a

  (** Set a value.

      @raise {Invalid_argument} if [i] is outside the range [0, (size t)). *)
  val set : 'a t -> int -> 'a -> 'a t

  (** Map the values, the size of the domain does not change. *)
  val map : 'a t -> eq:'b equality -> f:('a -> 'b) -> 'b t

  (** Merge partition maps.

    One can think of this as a "map2", but please see the note for why I chose
    to call them merges instead.

    [Merge] takes a specific [equality] predicate because it compresses new
    values generated by the mapping. When we compute a new value from the 2
    intersecting elements, we will scan an accumulator and add it if it is
    [not] equal to the other elements in the accumulator. Specifying, a good
    predicate for such an operation is important as it is intended to constrain
    the size of the final result.

    @raise Invalid_argument if the partition maps do not represent domains of the
    same size.
  *)
  val merge : 'a t
              -> 'b t
              -> eq:('c -> 'c -> bool)
              -> f:('a -> 'b -> 'c)
              -> 'c t

  (** Merge 3 partition maps, see [merge]. *)
  val merge3 : 'a t
              -> 'b t
              -> 'c t
              -> eq:('d -> 'd -> bool)
              -> f:('a -> 'b -> 'c -> 'd)
              -> 'd t

  (** Merge 5 partition maps, see [merge]. *)
  val merge4 : 'a t
              -> 'b t
              -> 'c t
              -> 'd t
              -> eq:('e -> 'e -> bool)
              -> f:('a -> 'b -> 'c -> 'd -> 'e)
              -> 'e t

  (* Fold over the values. *)
  val fold_values : 'a t
                  -> f:('b -> 'a -> 'b)
                  -> init:'b
                  -> 'b

  (* Fold over the values passing the underlying set to the lambda. *)
  val fold_set_and_values : 'a t
                          -> f:('b -> Set.t -> 'a -> 'b)
                          -> init:'b
                          -> 'b

  (** Fold over the indices [0,size) and values. *)
  val fold_indices_and_values : 'a t
                              -> f:('b -> int -> 'a -> 'b)
                              -> init:'b
                              -> 'b

  (* Iterate over the entries and values. *)
  val iter_indices_and_values : 'a t -> f:(int -> 'a -> unit) -> unit

  (* Return the values, in ascending order, in an array. *)
  val to_array : 'a t -> 'a array

  (* The size of the partition. Specifically, if [size t = n] then [get t i] will
    succeed for [0, n).  *)
  val size : 'a t -> int

  (* The number of unique elements in the underlying association list. *)
  val length : 'a t -> int

  (* Convert to Descending.t *)
  val descending : 'a t -> 'a Descending.t

  (* Cross pair the maps. *)
  val cpair : f:('a -> 'a -> 'b)
            -> eq:('b -> 'b -> bool)
            -> 'a t
            -> 'b t

end (* Ascending *)
