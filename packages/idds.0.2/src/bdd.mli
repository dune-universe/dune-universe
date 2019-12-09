(** Reduced Ordered Binary Decision Diagrams (BDDs). *)

(** {2 Types} *)

(** A BDD is just a DD with two additional structural constraints:
      - {b Ordered}: the variables along any root-leaf path of a BDD increase
        strictly monotonically.
      - {b Reduced}: The [hi] and [lo] subtrees of any branch are distinct. *)
type t = private Dd.t

val equal : t -> t -> bool

type manager

val manager : ?dd_mgr:Dd.manager -> unit -> manager

val get_dd_manager : manager -> Dd.manager


(** {2 Constructors} *)

val ctrue : t
val cfalse : t


(** {2 Boolean operations} *)

(** boolean conjunction (logical and) *)
val conj : manager -> t -> t -> t

(** boolean disjunction (logical or) *)
val disj : manager -> t -> t -> t

(** boolean negation (logical not) *)
val neg : manager -> t -> t

(** [ite mgr i u v] behaves like [u] when variable [i] is true, and like [v]
    otherwise.*)
val ite : manager -> Var.t -> t -> t -> t

(** [test mgr var b] is the diagram that behaves like [b] when [var = true] and
    like [not b] when [var = false] *)
val test : manager -> Var.t -> bool -> t

(** BDDs form a boolean algebra. *)
module Make () : Boolean.Algebra
  with type t = t


(** {2 Semantics} *)

val eval : t -> env:(Var.t -> bool) -> bool


