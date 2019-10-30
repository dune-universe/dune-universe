(** An interface for a type with a binary, associative operator over it and a
    privileged [unit] value.

    A {{!module-type:S} Monoid} is a {{!module-type:Semigroup.S} Semigroup}
    enriched an identity element, called {{!val:S.unit} unit}.

    Some well known monoids are

    - The integers under addition
      + [type t = int]
      + [op = (+)]
      + [unit = 0]
    - The integers under multiplication
      + [type t = int]
      + [op = ( * )]
      + [unit = 1]
    - Strings under concatenation
      + [type t = string]
      + [op = (^)]
      + [unit = ""] *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Monoid} for the type {{!type:Seed.t} t} *)
module type Seed = sig
  include Semigroup.Seed

  (** [unit] is the identity element in the monoid over {!type:t}.

      Every implementation must ensure that for any [x : t], {!val:unit} is the
      identity element under {!val:op}, thus that the equation [(x * unit) =
      (unit * x) = x] holds. *)
  val unit : t
end

(** {1 Interface} *)

(** A monoid is a set of objects with an associative binary operation over it
    and a privileged [unit] element that acts as an identity. *)
module type S = sig
  include Semigroup.S

  (** [unit] is the identity element in the monoid over {!type:t}.

      Every implementation must ensure that for any [x : t], {!val:unit} is the
      identity element under {!val:op}, thus that the equation [(x * unit) =
      (unit * x) = x] holds. *)
  val unit : t

  (** [mconcat xs] is the concatenation of all elements in [xs] into a single
      value, derived by use of [op].

      This is equivalent to [List.fold_right op xs unit].

      The initial {i m} marks this as monoidal concatenation rather than the
      {!val:concat} function inherited from the underlying
      {{!module-type:Semigroup.S} Semigroup}. Since {!val:mconcat} can use the
      {!val:unit} to prime the fold, it can work over a normal list, in contrast
      to {!val:concat}. *)
  val mconcat : t list -> t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Monoid} in the form of predicates that should hold true
    for any arguments of the appropriate type.

    You can use the [alg_structs_qcheck] package to generate property based
    tests of these laws for new modules satisfying this interface.

    @param M An implementation of {{!module-type: S} Monoid} *)
module Law (M : S) : sig

  (** From {!module:Semigroup.Law}

      [associativity x y z] is [true] when

      {[
        S.(x * (y * z)) = S.((x * y) * z)
      ]} *)
  val associativity : M.t -> M.t -> M.t -> bool

  (** [unit_right_cancelation x] is [true] when

      {[
        S.(x * unit) = unit
      ]} *)
  val unit_right_cancelation : M.t -> bool

  (** [unit_left_cancelation x] is [true] when

      {[
        S.(unit * x) = unit
      ]} *)
  val unit_left_cancelation : M.t -> bool

  (** [mconcat_is_a_fold_right xs] is [true] when

      {[
        (M.mconcat xs) = List.fold_right M.op xs M.unit
      ]} *)
  val mconcat_is_a_fold_right : M.t list -> bool
end

(** {1 Constructors}

    Module functors for creating implementations of {{!module-type:S}
    Monoid} *)

(** [Make S] is a implementation of {{!module-type:S} Monoid} grown from the
    implementation of the {!module-type:Seed}. *)
module Make (Seed : Seed) : S with type t = Seed.t

(** [make (unit : t) (op : t -> t -> t)] is an implementation of
    {{!module-type:S} Monoid} over the type [t] with the given [unit] and
    [op] *)
val make : 'a -> ('a -> 'a -> 'a) -> (module S with type t = 'a)

(** [of_semigroup semi unit] forms an implementation of {{!module-type:S}
    Monoid} by enriching the give semigroup [semi] with the [unit] *)
val of_semigroup : (module Semigroup.S with type t = 'a) -> 'a -> (module S with type t = 'a)

(** {1 Implementations} *)

(** Monoids over {!type:bool} *)
module Bool : sig

  (** [op] is [(||)] and [unit] is [false] ...

      The
      {{:https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Any}
      Haskell standard library} cals this "Any" since chaining [op] yields
      [true] if any operand is [true]. E.g.,

      {[
        # Monoid.Bool.Or.( true * false * false * false );;
        - : bool = true
      ]} *)
  module Or : S with type t = bool

  (** [op] is [(&&)] and [unit] is [true] *)
  module And : S with type t = bool
end

(** Monoids over {!type:int} *)
module Int : sig

  (** [op] is [(+)] and [unit] is 0 *)
  module Sum : S with type t = int

  (** [op] is [( * )] and [unit] is 1 *)
  module Product : S with type t = int
end

(** Semigroups over option types *)
module Option : sig
  module Make (S : Semigroup.S) : S with type t = S.t Option.t
end

(** See {!module:Semigroup.Endo} *)
module Endo : sig

  (** See {!module:Semigroup.Endo.Make} *)
  module Make (T : Triv.S) : S with type t = (T.t -> T.t)

  (** See {!val:Semigroup.Endo.make} *)
  val make : 'a Util.proxy -> (module S with type t = 'a -> 'a)
end

(** See {!module:Semigroup.Dual} *)
module Dual : sig

  (** See {!module:Semigroup.Dual.Make} *)
  module Make (M : S) : S with type t = M.t

  (** [dualize m] creates a first-class module that is the dual of the
      first-class module [m]. *)
  val dualize : (module S with type t = 'a) -> (module S with type t = 'a)
end
