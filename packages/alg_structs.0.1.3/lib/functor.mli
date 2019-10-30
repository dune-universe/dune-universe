(** An interface for types that can be mapped over.

    All you need to know to use this module effectively:

    given [type 'a t], function [map : f:('a -> 'b) -> 'a t -> 'b t] will {i
    map} [f] "over" [t] by taking a value of type ['a t] to a value of type ['b
    t].

    E.g., if

    {[type 'a t = int list]}

    then

    {[map ~f:Int.to_string : int t -> string t]}

    is a function which maps [int list]s to [string list]s, so

    {[map ~f:Int.to_string [1;2;3] = ["1";"2";"3"]]}

    {1 A Rough Sketch of the Category Theoretic Basis}

    {i To repeat, you don't need to read any of the following in order to make
    use of this module.}

    The use of the word {i functor} in this context refers to the category
    theoretic concept of {{:https://en.wikipedia.org/wiki/Functor} Functor},
    which is a map between categories. A functor [F: C -> D] that maps category
    [C] to category [D] consists of two mappings:

    + a mapping of each object in [C] to some object in [D]
    + a mapping of each arrow in [C] to some arrow in [D]

    These mappings must respect the {!module:Law}s.

    An "endofunctor" is a functor that is a map of one category within itself
    (or back onto itself).

    We imagine a category [CAML] (analogous to {{:https://wiki.haskell.org/Hask}
    Hask}), where the objects are OCaml types and the arrows are functions
    between those types. {!module-type:S} then specifies an interface for
    modules that implement an "endofunctor" on [CAML], mapping types to types
    and functions to functions. *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Functor}. *)
module type Seed = sig

  (** The principle type.

      The type constructor [t] is the mapping of objects taking every type ['a]
      to a type ['a t]. *)
  type 'a t

  (** [map ~f] maps the function [f : 'a -> 'b] to a function
      ['f T : 'a T -> 'b T].

      As an example, if [T (x : u) : u t] then [map ~(f:u -> v) (T x)] is [T (f
      x) : v t]. As a result, [map] is often thought of as applying [f] "in"
      [T].

      The function [map] is the mapping of arrows, taking every arrow ['a -> 'b]
      to an arrow ['a t -> 'b t]. *)
  val map : f:('a -> 'b) -> ('a t -> 'b t)
end

(** Interface for a mappable type with an unlabeled [map] function. *)
module type UnlabeledSeed = sig
  type 'a t

  (** Equivalent to {!val:S.map} but with an unlabeled function argument *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end


(** {1 Interface} *)

(** A module satisfying {!module-type:S} is an implementation of a functor.  *)
module type S = sig

  include Seed

  (** Infix for {!val:map} *)
  val ( <@> ) : ('a -> 'b) -> 'a t -> 'b t

  (** Mapped version of [|>] (which is flipped {!val:(<&>)}) *)
  val ( |>> ) :  'a t -> ('a -> 'b) ->'b t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Functor} in the form of predicates that should be true
    for any arguments of the appropriate type.

    See {{:https://en.wikipedia.org/wiki/Functor#Definition} wikipedia's
    definition functors} for the mathematical expression of these properties.

    You can use {!module:Alg_qcheck.Functor} to generate property based tests of
    these laws for new modules satisfying this interface.

    @param F An implementation of a {{!module-type:S} Functor} *)
module Law (F : S) : sig

  (** [identity x] is [true] when

      {[
        F.map ~f:Fun.id x = Fun.id x
      ]} *)
  val identity : 'a F.t -> bool

  (** [composition f g x] is [true] when

      {[
        F.map ~f:(f % g) x = (F.map ~f % F.map ~f:g) x
      ]}

      where [%] is composition. *)
  val composition : ('a -> 'b) -> ('c -> 'a) -> 'c F.t -> bool
end

(** {1 Constructors}

    Module functors and signatures for expediting instantiation of
    {{!module-type:S} Functors}. *)

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t

(** [MakeUnlabeled] makes an module instantiating {!module-type:S}, with a labeled
    {!val:S.map}, out of a module instantiating {!module-type:UnlabeledSeed}
    with an unlabeled [map] function. *)
module MakeUnlabeled (Seed : UnlabeledSeed) : S with type 'a t = 'a Seed.t

(** {1 Implementations} *)

module Option : S with type 'a t = 'a Option.t
module List : S with type 'a t = 'a List.t
module Array : S with type 'a t = 'a Array.t
