(** Identity Suppressed Decision Diagrams (IDDs). *)

(** {2 Types} *)

(** An IDD is just a DD with two additional structural constraints that ensure
    canonicity: two IDDs encode the same function iff they are the same IDD. *)
type t = private Dd.t

type manager

val manager : unit -> manager

(** {2 Constructors} *)

(** The identity relation. *)
val ident : t

(** The empty relation. *)
val empty : t

(** For i < n,
  eval (test i b) env n =
    env (Var.inp i) = b && eval ident env n
  rel n (test i b) = \{ (x,x) | x \in B^n, x_i = b \}
     *)
val test : manager -> int -> bool -> t

(** For i < n,
  eval (set i b) env n = eval ident env' n, where
    env' x = if x = Var.inp i then b else env x
  rel n (set i b) = \{ (x,x[i:=b]) | x \in B^n \}
  *)
val set : manager -> int -> bool -> t

(** [branch mgr var hi lo] is the diagram that behaves like [hi] when
    [var = true], and like [lo] when [var = false]. *)
val branch : manager -> Var.t -> t -> t -> t

(** [apply mgr op t0 t1] is [t] such that
    [Bool.equal (op (eval t0 env n) (eval t1 env n)) (eval t env n)] *)
val apply : manager -> (bool -> bool -> bool) -> t -> t -> t

(** Sequential composition *)
val seq : manager -> t -> t -> t

(** (Relational) union *)
val union : manager -> t -> t -> t

(** (Relational) transitive-reflexive closure  *)
val star : manager -> t -> t

(** {2 Boolean operations} *)

(** O(1) structural equality.

    {b PRECONDITION}: The result of [equal u v] is only defined when [u] and [v]
    were built using the same manager. Otherwise, the result is arbitrary. *)
val equal : t -> t -> bool

(** relational containment *)
val subseteq : manager -> t -> t -> bool

(** {2 Semantics} *)

(** [eval tree env n] evaluates idd [tree] in environment [env] where the
    variable indices are 0,...,[n-1] *)
val eval : t -> (Var.t -> bool) -> int -> bool

module Rel : Algebra.KAT with
  type b := Bdd.t and
  type t := t
