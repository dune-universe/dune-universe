(** Just a type `t`. Used for
- the type of the environment in {!module:Applicative.Env},
- the type of the accumulator in {!module:Applicative.Fold},
- the error type in {!module:Applicative.Result}. *)
module type TypeS = sig
  type t
end

(** {!modtype:Monoid.S} is a signature for a monoid
    (a type `t` with `zero` and `+`),
    and ['a] {!type:Monoid.t} is the type for monoids represented as
    first-class modules. *)
module Monoid : sig
  module type S = sig
    type t

    val zero : t

    val ( + ) : t -> t -> t
  end

  type 'a t = (module S with type t = 'a)
end

(** {!modtype:Functor.S} is a signature for a functor
    (a type ['a] `t` with `map`). *)
module Functor : sig
  module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end
end

(** {!modtype:Applicative.S} is a signature for an applicative functor
    (a type ['a] `t` with `map`, `pure` and `apply`).
    This module contains many instances of applicative functors. *)
module Applicative : sig
  module type S = sig
    include Functor.S

    val pure : 'a -> 'a t

    val apply : ('a -> 'b) t -> (unit -> 'a t) -> 'b t
    (** The second argument is delayed for its evaluation to be skipped
        if not necessary: it allows short-circuits with {!val:forall},
        {!val:exists}, etc. *)
  end

  module Iter : S with type 'a t = unit

  module Map : S with type 'a t = 'a

  module Reduce (Monoid : Monoid.S) : S with type 'a t = Monoid.t

  module Env (E : TypeS) (Base : S) : S with type 'a t = E.t -> 'a Base.t

  module Fold (Accu : TypeS) : S with type 'a t = Accu.t -> Accu.t

  module Pair (U : S) (V : S) : S with type 'a t = 'a U.t * 'a V.t

  module Forall : S with type 'a t = bool

  module Exists : S with type 'a t = bool

  module Option (Base : S) : S with type 'a t = 'a Base.t option

  module Result (Base : S) (Err : TypeS) : S
  with type 'a t = ('a Base.t, Err.t) result

  module List (Base : S) : S with type 'a t = 'a Base.t list
end

(** Traversal for abstract sequences. *)
module type SequenceSpecS = sig
  type 'a t

  type 'a desc =
    | Nil
    | Cons of 'a * 'a t

  val destruct : 'a t -> 'a desc

  val construct : 'a desc -> 'a t
end

module type SequenceS = sig
  type 'a s

  module Arity : sig
    type ('a, 'a_t, 'f, 'result, 'is_empty) t =
      | O : ('a, 'a_t, 'a, 'a_t, [`Empty]) t
      | S : ('a, 'a_t, 'f, 'result, _) t ->
          ('a, 'a_t, 'x -> 'f, 'x s -> 'result, [`Not_empty]) t
  end

  module Make (Applicative : Applicative.S) : sig
    val traverse :
        ('a Applicative.t, 'a s Applicative.t, 'f, 'result,
         [`Not_empty]) Arity.t -> 'f -> 'result
  end
end

module Sequence (Spec : SequenceSpecS) : SequenceS with type 'a s = 'a Spec.t

(** Traversal for lists. *)
module List : SequenceS with type 'a s = 'a list

(** Traversal for sequences. *)
module Seq : SequenceS with type 'a s = 'a Seq.t
