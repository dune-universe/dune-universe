(** Data structures that can be folded over. *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Foldable} *)
module type Seed = sig

  (** The principle type, which can be folded over using {!val:fold_right}. *)
  type 'a t

  (** [fold_right ~f t ~init] combines the elements of [t] using [f] from the
      right. *)
  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
end

(** {1 Interface} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Foldable} for the type {{!type:Seed.t} t}. *)
module type S = sig
  include Seed

  (** [fold m t] folds the elements of [t] with [m.op]. E.g.,

      {[
        # Foldable.List.fold (module Monoid.Int.Sum) [1;2;3;4];;
        - : int = 10
      ]} *)
  val fold : (module Monoid.S with type t = 'a) -> 'a t -> 'a

  (** [fold_map ~m ~f t] is a map of the elements of [t] in the monoid [m]
      combines with [m.op].

      E.g.,

      {[
        # Foldable.List.fold_map ~m:(module Monoid.Int.Sum) ~f:int_of_string ["1";"2";"3";"4"];;
        - : int = 10
      ]} *)
  val fold_map : m:(module Monoid.S with type t = 'm) -> f:('a -> 'm) -> 'a t -> 'm

  (** [fold_left ~f ~init t] combines the elements of [t] using [f] from the
      left. *)
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t ->  'b

  (** [to_list t] is a list with the elements of [t] from left to right*)
  val to_list : 'a t -> 'a list

  (** [is_empty t] is [true] when [t] is empty *)
  val is_empty : 'a t -> bool

  (** [length t] is the number of elements in [t] *)
  val length : 'a t -> int

  (** [any ~f t] is [true] if the predicate [f] holds for any element of [t] *)
  val any : f:('a -> bool) -> 'a t -> bool

  (** [all ~f t] is [true] if the predicate [f] holds for every element of [t] *)
  val all : f:('a -> bool) -> 'a t -> bool

  (** [mem t x ~equal] is true if there's an element [y] in [t] such that [equal
      x t] is [true] *)
  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

  (* TODO find*)

  (** [max ~compare t] is [Some x] when [x] is the maximum value in [t], as
      determined by [compare], or [None] if [is_empty t]. *)
  val max : compare:('a -> 'a -> int) -> 'a t -> 'a option

  (** [min ~compare t] is [Some x] when [m] is the minimum value in [t], as
      determined by [compare], or [None] if [is_empty t]. *)
  val min : compare:('a -> 'a -> int) -> 'a t -> 'a option
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Foldable} in the form of predicates that should hold true
    for any arguments of the appropriate type.

    You can use the [alg_structs_qcheck] package to generate property based
    tests of these laws for new modules satisfying this interface.

    @param F An implementation of {{!module-type: S} Foldable} *)
module Law (F : S) : sig

  (** [fold_right f init t] is [true] when

      {[
        F.fold_right ~f ~init t = (F.fold_map ~m ~f t) init
      ]}

      where [init] has type [a] and [m] is the {{!module:Monoid.Endo} Endo
      monoid} over functions of type [a -> a]. *)
  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a F.t -> bool


  (** [fold_left f init t] is [true] when

      {[
        F.fold_right ~f ~init t = (F.fold_map ~m ~f:(Fun.flip f) t) init
      ]}

      where [init] has type [a] and [m] is the {{!module:Monoid.Dual} Dual} of
      the {{!module:Monoid.Endo} Endo monoid} over functions of type [a ->
      a]. *)
  val fold_left : ('a -> 'a -> 'a) -> 'a -> 'a F.t -> bool


  (** [fold (module M) t] is [true] when

      {[
        F.fold (module M) t = F.fold_map ~m:(module M) ~f:Fun.id t
      ]}*)
  val fold : (module Monoid.S with type t = 'a) -> 'a F.t -> bool

  (** [length t] is [true] when

      {[
        F.length t = F.fold_map ~m ~f:(Fun.const 1) t
      ]}

      where [m] is {!module:Monoid.Int.Sum}. *)
  val length : 'a F.t -> bool
end

(** {1 Constructors}

    Module functors for creating implementations of {{!module-type:S}
    Foldable} *)
module Make (Seed : Seed) : S with type 'a t = 'a Seed.t

(** {1 Implementations} *)

(** Folds over optional values...

    [Option.fold_right ~f t ~init] is [f x init] if [t] is [Some x] or [init] if
    [t] is [None]. *)
module Option : S with type 'a t = 'a Option.t

(** Implemented by [Stdlib.ListLabels]. *)
module List : S with type 'a t = 'a List.t

(** Implemented by [Stdlib.ArrayLabels]. *)
module Array : S with type 'a t = 'a Array.t
