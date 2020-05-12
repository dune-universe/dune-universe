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

  module Addition : S

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
    (** The evaluation of the second argument is delayed so that it
        can be skipped if not necessary: it allows short-circuits with
        {!val:forall}, {!val:exists}, etc. *)
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

type ('a, 'b) length =
  | Zero : (unit, unit) length
  | Succ : ('a, 'b) length -> (_ * 'a, _ * 'b) length

module type SequenceOfUnaryTypeS = sig
  type 'item x

  type 'sequence t =
    | [] : unit t
    | (::) : 'hd x * 'tl t -> ('hd * 'tl) t
end

module rec Sequence : SequenceOfUnaryTypeS with type 'a x = 'a

module type SequenceOfBinaryTypeS = sig
  type ('a, 'b) x

  type ('a_s, 'b_s) t =
    | [] : (unit, unit) t
    | (::) : ('a, 'b) x * ('a_s, 'b_s) t -> ('a * 'a_s, 'b * 'b_s) t
end

module Arity : sig
  module type S = sig
    type ('a, 'b) t

    module ArrowSequence : SequenceOfBinaryTypeS with
    type ('a, 'b) x = ('a, 'b) t -> 'b

    val destruct :
        ('a, 'b) length ->
        ('c -> 'a Sequence.t) ->
        (('a, 'b) ArrowSequence.t -> 'd) ->
        ('c, 'd) t
  end

  module type NonNullS = sig
    module Pred : S

    include S with type ('a, 'b) t = 'a -> ('a, 'b) Pred.t
  end

  module O : S with type ('a, 'b) t = 'b

  module S (Pred : S) : NonNullS with module Pred = Pred

  module A1 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) O.t

  module A2 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A1.t

  module A3 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A2.t

  module A4 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A3.t

  module A5 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A4.t

  module A6 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A5.t

  module A7 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A6.t

  module A8 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A7.t

  module A9 : NonNullS with type ('a, 'b) Pred.t = ('a, 'b) A8.t
end

exception StructuralMismatch
