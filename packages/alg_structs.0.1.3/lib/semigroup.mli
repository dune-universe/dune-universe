(** An interface for a type with a binary, associative operator over it.

    "A semigroup is an algebraic structure consisting of a set together with an
    associative binary operation" ({{:https://en.wikipedia.org/wiki/Semigroup}
    wikipedia)}.

    "The term 'semigroup' is standard, but semi-monoid would be more
    systematic." {{:https://ncatlab.org/nlab/show/semigroup} ncatlab}

    Modules that implement the semigroup interface are a structural subtype of
    the better known {{!module-type:Monoid.S} Monoids} interface. Semigroups are
    differentiated from Monoids by the absence of a unit (or identity) element.
    in their specification. *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Semigroup} for the type {{!type:Seed.t} t}. *)
module type Seed = sig

  (** The principle (and sole) type.

      We can think of this set-theoretically as the carrier set of the algebraic
      structure or category-theoretically as the single object in the category,
      with each element being a morphism [t -> t]. *)
  type t

  (** [op x y] is an associative operation over all elements [x] and [y] of type
      {!type:t}. Category-theoretically, this is the composition of
      morphisms. *)
  val op : t -> t -> t
end

(** As {!module-type:Seed} but for parameteric types of one variable *)
module type Seed1 = sig
  type 'a t

  (** [op x y] is an associative operation over all elements [x] and [y] of type
      {!type:t} *)
  val op : 'a t -> 'a t -> 'a t
end

(** {1 Interface} *)

(** A semigroup is a set of objects with an associative binary operation over it *)
module type S = sig
  include Seed

  (** The infix version of {!val:op}. *)
  val ( * ) : t -> t -> t

  (** [concat xs] is the concatenation of all elements of [xs] into a single
      value using [op].

      This is equivalent to [List.fold_right op (NonEmptyList.tl xs)
      (NonEmptyList.hd xs)]. *)
  val concat : t NonEmptyList.t ->  t
end

module type S1 = sig
  include Seed1

  (** The infix for {!val:op}. *)
  val ( * ) : 'a t -> 'a t -> 'a t

  (** [concat xs] is the concatenation of all elements of [xs] into a single
      value using [op].

      This is equivalent to [List.fold_right op (NonEmptyList.tl xs)
      (NonEmptyList.hd xs)]. *)
  val concat : 'a t NonEmptyList.t -> 'a t
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Semigroup} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use the [alg_structs_qcheck] package to generate property based
    tests of these laws for new modules satisfying the interface.

    @param S An implementation of a {{!module-type: S} Semigroup} *)
module Law (S : S) : sig

  (** [associativity x y z] is [true] when

      {[
        S.(x * (y * z)) = S.((x * y) * z)
      ]} *)
  val associativity : S.t -> S.t -> S.t -> bool
end

(* TODO S2 for monoids over parametric types *)

(** {1 Constructors}

    Functions and module functors for creating implementations of
    {{!module-type:S} Semigroups} *)

(** [Make (S)] is an implementation of {{!module-type:S} Semigroup} generated
    from the {!module-type:Seed}. *)
module Make (S:Seed) : S with type t = S.t

(** [make op] is an implementation of {{!module-type:S} Semigroup} generated
    from the operation [op]. *)
val make : ('a -> 'a -> 'a) -> (module S with type t = 'a)

(** {1 Implementations} *)

(** Semigroups over {!type:bool} *)
module Bool : sig

  (** [op] is [(||)] *)
  module Or : S with type t = bool

  (** [op] is [(&&)] *)
  module And : S with type t = bool
end

(** Semigroups over {!type:int} *)
module Int : sig

  (** [op] is [(+)] *)
  module Sum : S with type t = int

  (** [op] is [( * )]*)
  module Product : S with type t = int
end

(** Semigroups over option types *)
module Option : sig

  (** [Make (S)] is a semigroup where [op a b] is...

      - [None] if both [a] and [b] are [None]
      - [Some v] if only one of [a] or [b] are [Some v]
      - [Some (S.op a' b')] if [b] is [Some b'] and [a] is [Some a']

      This enables chains of associations over optional values that preserves
      any values that may be present. E.g.,

      {[
        # module O = Semigroup.Option.Make ((val Semigroup.make ( * )));;
        module O :
        sig
          type t = int option
          val op : t -> t -> t
          val ( * ) : t -> t -> t
          val concat : t NonEmptyList.t -> t
        end

        # O.(Some 2 * None * None * Some 2);;
        - : O.t = Option.Some 4
      ]}

      @param S An implementation of {{!module-type:S} Semigroup}
  *)
  module Make (S : S) : S with type t = S.t Option.t
end

(** [Endo] is a semigroup where the operator is the composition of functions
    with input and output of the same type.

    Or, to paraphrase the
    {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html#t:Endo}
    Haskell docs}, [Endo] implements "the semigroup of endomorphisms under
    composition". "Endomorphism" just meaning a morphism with the same object
    for its source and target, i.e., (here) a function with input and output of
    same type.

    E.g. using the first-order module generator {!val:Endo.make}, we can make the
    [Endo] semigroup over functions of type [string -> string] thus:

    {[
      # module E = (val Semigroup.Endo.make "");;
      module E :
      sig
        type t = string -> string
        val op : t -> t -> t
        val ( * ) : t -> t -> t
        val concat : t NonEmptyList.t -> t
      end;;

      # let comp = E.( (fun y -> "Hello, " ^ y) * (fun x -> x ^ "!") );;
      val comp : E.t = <fun>;;

      # comp "OCaml";;
      - : string = "Hello, OCaml!"
    ]} *)
module Endo : sig

  (** [Make (T)] is a module implementing the [Endo] semigroup for functions
      over type [T.t] *)
  module Make (T : Triv.S) : S with type t = (T.t -> T.t)

  (** [make (Proxy : t Util.proxy)] is a first-class module implementing the
      [Endo] semigroup for functions [(t -> t)].

      Note that [Proxy] is used only to convey the type. See {!type:Util.proxy}.

      You can lift the result back into the module like so:

      {[
        # module E = (val Semigroup.Endo.make (Util.Proxy : int proxy));;
        module E :
        sig
          type t = int -> int
          val op : t -> t -> t
          val ( * ) : t -> t -> t
          val concat : t NonEmptyList.t -> t
        end
      ]} *)
  val make : 'a Util.proxy -> (module S with type t = 'a -> 'a)
end

(** [Dual] allows constructing the dual semigroup for a given semigroup.
    I.e., a semigroup with the arguments of it's operator reversed. *)
module Dual : sig

  (** [Make (S)] is [S] except that [S.op] is defined as [Fun.flip S.op]. *)
  module Make (S : S) : S with type t = S.t

  (** [make op] is [Semigroup.make (Fun.flip op)]. *)
  val make : ('a -> 'a -> 'a) -> (module S with type t = 'a)
end
