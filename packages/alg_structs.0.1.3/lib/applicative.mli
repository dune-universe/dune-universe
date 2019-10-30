(** Applicative {{!module:Functor} functors} "allow sequencing of functorial
    computations" ({{:https://en.wikipedia.org/wiki/Applicative_functor}
    wikipedia}) with the limitation that "values computed by subcomputations
    cannot affect what subsequent computations will take place"
    ({{:https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base__/Applicative_intf/index.html}
    Core docs}).

    Assume that we've implemented {{!module-type:S} Applicative} for some
    parametertic type ['p t]. Take two values [F x : 'a t] and [F y : 'b t] and
    a computation represented by a function [F f : ('a -> 'b -> 'c) t]. We can
    then use the applicative's namesake function {!val:S.apply} to sequence this
    computation "within" type [t]:

    [apply (apply f x) y : 'c t]

    The "sequencing" of the computation (via application) is even more apparent
    when we use the infix notation for [apply]:

    [f <*> x <*> y]

    See {{!section:implementations} Implementations} for illustrative examples. *)

(** {1 Seed} *)

(** The [Seed] needed to generate an implementation of {{!module-type:S}
    Applicative} *)
module type Seed = sig
  include Functor.S

  (** [return x] is the minimal value of [x] in {!type:t} *)
  val return : 'a -> 'a t

  (** [apply (F f) (F x)] is [F (f x)], i.e., [f] applied to [x] in {!type:t} *)
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

(** {1 Interface} *)

module type S = sig
  include Seed

  (** [map2 ~f x y] is [return f <*> x <*> y], i.e., [f] "lifted" in to
      {!type:t} and then applied to both [x] and [y]. *)
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  (** [(F f) <*> (F x)] is [F (f x)] *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (** [a *> b] sequences actions [a] and [b] but discards the value of [a] *)
  val ( *> )  : 'a t -> 'b t -> 'b t

  (** [a <* b] sequences actions [a] and [b] but discards the value of [b] *)
  val ( <* )  : 'a t -> 'b t -> 'a t

  (** [both (F a) (F b)] is [F (a, b)], i.e., it is the product of [a] and [b]
      in [F] *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** Binding operators for use with the OCaml 4.08 extension. See the
      {{:https://caml.inria.fr/pub/docs/manual-ocaml/manual046.html} OCaml docs
      on binding operators} for more information. *)
  module Let_bind : sig

    (** [let+ x = t in f x] is [map ~f t] *)
    val (let+) : 'a t -> ('a -> 'b) -> 'b t

    (** [let+ x = t and+ y = t' in f x y] is [f <@> t <*> t'] *)
    val (and+) : 'a t -> 'b t -> ('a * 'b) t
  end
end

(** {1 Laws} *)

(** [Law] notes the laws that should be obeyed by any instantiation of
    {{!module-type:S} Applicative} in the form of predicates that should be true
    for any arguments of the appropriate type.

    You can use {!module:Alg_qcheck.Applicative} to generate property based
    tests of these laws for new modules satisfying this interface.

    @param A An implementation of {{!module-type: S} Applicative} *)
module Law (A : S) : sig

  (** [identity x]: [return Fun.id <*> x = x] *)
  val identity : 'a A.t -> bool

  (** [composition u v w]: [(return (%) <*> u <*> v <*> w) = (u <*> (v <*> w))] *)
  val composition : ('b -> 'c) A.t -> ('a -> 'b) A.t -> 'a A.t -> bool

  (** [homomorphism f x]: [return f <*> return x = return (f x)] *)
  val homomorphism : ('a -> 'b) -> 'a -> bool

  (** [interchange u y]: [(u <*> return y) = (return (fun f -> f y) <*> u)] *)
  val interchange : ('a -> 'b) A.t -> 'a -> bool
end

(** {1 Constructors}

    Module functors for creating implementations of {{!module-type:S}
    Applicative} *)

module Make (B : Seed) : S with type 'a t = 'a B.t

(** {1 Implementations}

    Implementations for common data structures. See the documentation for each
    implementation module for an illustrative example. *)

(** [Option] provides sequencing of partial computations. E.g,

    [# Option.((+) <@> Some 5 <*> Some 7);;]

    [- : int option = Option.Some 12]

    [# Option.((+) <@> None <*> Some 5);;]

    [- : int option = Option.None] *)
module Option : S with type 'a t = 'a Option.t

(** [List] defines applications that work across all possible
    combinations of the items in the respective lists. This is often used to
    model non-determinism.

    E.g., to get all pairs that can be formed by selecting first and second
    elements from the respective lists:

    [# let x = List.NonDet.((fun x y -> (x, y)) <@> [1;2] <*> ['a';'b']);;]

    [- : (int * char) list = [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')] ] *)
module List : S with type 'a t = 'a List.t

(** As {!module:List} but using arrays. *)
module Array  : S with type 'a t = 'a Array.t
