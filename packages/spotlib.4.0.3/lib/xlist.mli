(** {2 Type} *)

type 'a t = 'a list

(** {2 Construction } *)

val empty : 'a t
(** [empty = []] *)

val singleton : 'a -> 'a t
(** [singleton x = [x]] *)

val make : int -> 'a -> 'a t
(** [make n x] returns a list of length [n] filled with [x].
    Throws [Invalid_argument "List.make"] when [n < 0].
*)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] returns a list of [[f 0; f 1; ..; f (n-1)]].
    Throws [Invalid_argument "List.init"] when [n < 0].
*)

(** {2 Conversions} *)

val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t

(** {2 Basics} *)

val append : 'a t -> 'a t -> 'a t
(** Tail recursive version of [List.append] *)

val (@) : 'a t -> 'a t -> 'a t
(** Tail recursive version of [List.(@)] *)

val concat : 'a t t -> 'a t
(** Tail recursive version of [List.concat] *)

val flatten : 'a t t -> 'a t
(** Tail recursive version of [List.flatten] *)

val replicate : 'a t -> int -> 'a t
(** [replicate xs n] concatenates the list [xs] [n]-times *)

(** {2 Folding} *)

val map : ('a -> 'b) -> 'a list -> 'b list

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

val split        : ('a * 'b) list -> 'a list * 'b list
val merge        : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

val fold_right : ('a -> 'st -> 'st) -> 'a t -> 'st -> 'st

val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
(** List must be non-empty.
    Otherwise, it raises [Invalid_argment "fold_left1"]. *)

val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
(** List must be non-empty.
    Otherwise, it raises [Invalid_argment "fold_left1"]. *)

val concat_map : ('a -> 'b list) -> 'a list -> 'b list
(** [concat_map f xs = concat @@ map f xs] but much faster. *)

val rev_concat_map : ('a -> 'b list) -> 'a list -> 'b list
(** [concat_map f xs = rev @@ concat @@ map f xs] but much faster. *)

val map_accum_left : ('acc -> 'a -> ('acc * 'b)) -> 'acc -> 'a t -> ('acc * 'b list)
(** [mapAccumL f acc t] behaves like a combination of [map] and [fold_left]; it applies a function [f] to each element of a list [t], passing an accumulating parameter [acc] from left to right, and returning a final value of this accumulator together with the new list.  *)

(** {2 Access} *)

val last : 'a list -> 'a
(** The last element of the list. Raises Failure when the argument is [].
    [last [1;2;3] = 3]
*)

(** {2 Find and assoc} *)

val find_opt : ('a -> bool) -> 'a list -> 'a option
val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option
val remove_first_match : ('a -> bool) -> 'a list -> 'a list

val assoc_all : 'a -> ('a * 'b) list -> 'b list
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq  : 'a -> ('a * 'b) list -> ('a * 'b) list

(** {2 Sublist by index} *)

val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list

val sub_default : 'a list -> int -> int -> 'a list
(** [sub_default t pos len] returns a sublist of [t] from the position [pos]
    (0-start) and length [len].

    The region specified by [pos] and [len] is trimmed by [t]: if it exceeds
    the size of [t], it is intersected with the region from [0]
    and length [List.length t]. One tricky example is:

      [sub_default [0;1;2;3;4;5] (-1)   3  = [0;1]]

*)

val take_exn : int -> 'a list -> 'a list
val drop_exn : int -> 'a list -> 'a list
val sub : 'a list -> int -> int -> 'a list

val split_at : int -> 'a list -> 'a list * 'a list
(** Haskell's [splitAt]. Always succeeds. *)
  
val splits_by : int -> 'a list -> 'a list list
(** Split a list into sub-lists of the fixed length *)

(** {2 Sublist by predicate} *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list

val span : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [span p xs] extract the longest prefix of [xs] whose elements
    satisfy [p].
*)

val partition_map : ('a -> [< `Left of 'left | `Right of 'right]) -> 'a list -> ('left list * 'right list)

(** {2 Sort and uniq} *)

val uniq_dup : ('a -> 'a -> bool) -> 'a list -> 'a list * ('a * 'a) list
(** Filter out duplicate elements using the given equality.
    The first result list is the list of first unique occurrences,
    the second result list is the rest, the duplications removed
    from the first result list, paired with the corresponding element
    of the first result list.

    O(n^2).
*)

val uniq_dup_sorted : ('a -> 'a -> int) -> 'a list -> 'a list * ('a * 'a) list
(** Same as [uniq_dup] but only works for already sorted by the same ordering.
    O(n log n)
*)

val unique : 'a list -> 'a list (** Haskell's nub. O(n^2) *)

val unique_by : ('a -> 'a -> bool) -> 'a list -> 'a list (* O(n^2) *)

val has_dup : ('a -> 'a -> bool) -> 'a list -> ('a * 'a) option
(** Check the list is a unique list or not, wrt the equality function.
    If not, it returns a dupe example.

    [has_dup (fun x y -> fst x = fst y) [(1,2); (2,3); (4,5)] = None]
    [has_dup (fun x y -> fst x = fst y) [(1,2); (2,3); (2,5)] = Some ( (2,3), (2,5) ) ]

    O(n^2)
*)

val group : 'a list -> 'a list list
(** [group xs] returns a list of lists such that the concatenation of the result is equal to the argument. Moreover, each sublist in the result contains only equal elements:

  [ group ['M'; 'i'; 's'; 's'; 'i'; 's'; 's'; 'i'; 'p'; 'p'; 'i'] = [['M'],['i'],['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']] ]

 Haskell's [group].
*)

val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Same as [group] but equality can be given. Haskell's [groupBy] *)

val sort_then_group_by : ('a -> 'a -> int) -> 'a list -> 'a list list
(** [sort] then [group_by] *)

(** {2 Composition} *)

val zip : 'a list -> 'b list -> ('a * 'b) list
(** Haskell's zip. Like List.combine but does not raise
    when the lengths are not equal. Tail recursive. *)

(** {2 With reference} *)

val accum  : 'a list ref -> 'a -> unit
(** [accum xsref x] is equivalent with [xsref := x :: !xsref] *)

val (+::=) : 'a list ref -> 'a -> unit
(** Same as [accum] *)

(** {2 Integer ranges} *)

val from_to : int -> int -> int list
(** [from_to f t = \[f..t\]] *)

val (--) : int -> int -> int list
(** Same as from_to. [f--t = \[f..t\]] *)

val init_from_to : int -> int -> (int -> 'a) -> 'a list
(** [init_from_to f t fn = \[fn x | x <- \[f..t\] \]] *)

(** {2 Misc} *)

val intersperse : 'a -> 'a list -> 'a list

val sum : int list -> int


(** {2 Modules} *)

module Infix : sig
  val (--) : int -> int -> int list
  (** [same as from_to. [f--t = [f..t]] ] *)

  val (+::=) : 'a list ref -> 'a -> unit
  (** Same as [accum] *)
end

module Pervasives : sig
  val (--) : int -> int -> int list
  (** [same as from_to. [f--t = [f..t]] ] *)

  val (+::=) : 'a list ref -> 'a -> unit
  (** Same as [accum] *)
end

(** {2 Non tail recursive versions}

    These non tail recursive versions are from stdlib.
*)

val (@.) : 'a t -> 'a t -> 'a t
(** Non tail recursive version of [(@)] *)

val append_ntr       : 'a t -> 'a t -> 'a t
val concat_ntr       : 'a t t -> 'a t
val flatten_ntr      : 'a t t -> 'a t
val map_ntr          : ('a -> 'b) -> 'a t -> 'b t
val mapi_ntr         : (int -> 'a -> 'b) -> 'a t -> 'b t
val fold_right_ntr   : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val map2_ntr         : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_right2_ntr  : ('a -> 'c -> 'b -> 'b) -> 'a list -> 'c list -> 'b -> 'b
val remove_assoc_ntr : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq_ntr  : 'a -> ('a * 'b) list -> ('a * 'b) list
val split_ntr        : ('a * 'b) list -> 'a list * 'b list
val combine_ntr      : 'a list -> 'b list -> ('a * 'b) list
val merge_ntr        : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

(** {2 Deprecated} *)

val iter_until : ('a -> [`Break of 'b | `Continue]) -> 'a list -> 'b option
  [@@ocaml.deprecated "Use Base.loop instead."];;

val scani_left :
  (int -> 'a -> 'b -> [< `Continue of 'a | `Stop of 'a ])
  -> 'a -> 'b list -> 'a
  [@@ocaml.deprecated "Use Base.loop instead."]

val is_unique : ('a -> 'b) -> 'a list -> ('a * 'a) option
[@@ocaml.deprecated "Use has_dup"]
(** Check the list is a unique list or not, wrt the key function.
    If not, it returns a dupe example.

    [is_unique fst [(1,2); (2,3); (4,5)] = None]
    [is_unique fst [(1,2); (2,3); (2,5)] = Some ( (2,3), (2,5) ) ]
*)

val sort_then_group : ('a -> 'a -> int) -> 'a list -> 'a list list
[@@ocaml.deprecated "Use sort_then_group_by"];;
