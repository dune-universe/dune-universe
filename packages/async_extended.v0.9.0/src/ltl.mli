open! Core
open! Async

(** Using linear temporal logic (LTL) to run online queries on sequences of
    states. A state is a record and the formula talks about values of the fields
    in this record.

    Given an LTL formula [phi] with variables [var(phi)], an assignment [E] of values
    to variables in [var(phi)], and a sequence of states [xs], there is a
    satisfaction relation [xs |= phi, E]. The function [Naive.eval] gives the
    precise definition of this relation.

    Given a formula [phi] and a pipe of states [input] the function [query]
    returns a pipe of all variable assignments [E] such that [input |= phi, E]
    (some assigment sets may be given in an implicit form - see documentation of
    [query]). Often a satisfying assignment can be determined without waiting for
    the end of the input pipe.

    [query phi input] works by keeping track of a set of constraints of the form
    [(phi', E')] such that any variable assignment [E] that satisfies [phi] and
    has not been returned yet must refine [E'] and satisfy [xs |= phi', E'] where
    [xs] is the remaining part of [input]. See the top of .ml for more details.

    We use an optimization that allows many useful queries to run in linear time
    in the size of the input. If we detect that a particular constraint [(phi,
    E)] is not going to change unless a particular field has a particular value,
    we put the constraint to sleep in a hashtable with the [(field, value)] pair
    as the key. You can check whether the optimization works as expected in your
    case by using the [?debug] argument of [query] with [`Info] level and
    checking in the output that the number of active constraints remains
    constant. See the top of .ml for more details.

    The worst-case runtime of the query is super-exponential in the size of the
    formula. This is unlikely to be a problem in practice.

    See base/async/extended/examples/ltl.ml for an example of use.
*)
module type State = sig
  type t
  val to_string : t -> string
  val time : t -> Time_ns.t option
end

(* LTL checker over sequences of State.t *)
module Make (State : State) : sig
  module Field : sig
    (* Due to the use of [Univ_map] internally, the ['a] should be
       polymorphically comparable. *)
    type 'a t

    val create
      :  name:string
      -> get:(State.t -> 'a option)
      -> 'a Hashtbl.Hashable.t
      -> 'a t

    val time : Time_ns.t t
  end

  module Variable : sig
    type 'a t [@@deriving compare]

    (** [create ?sexp_of name] creates a variable named [name]. The name is only
        used for debugging and error printing purposes. Every variable is unique,
        even if created with the same name.  *)
    val create : ?sexp_of:('a -> Sexp.t) -> string -> 'a t

    val to_string : 'a t -> string
  end

  module Assignment : sig
    type t
    val find     : t -> 'a Variable.t -> 'a option
    val find_exn : t -> 'a Variable.t -> 'a
  end

  module Expression : sig
    (** ['a t] can be applied to a state and a variable assignment to return
        ['a] or fail. The reason we wrap this up in an opaque type is that our solver
        keeps track of the variables used in each expression.

        See documentation of the [predicate] function below for an example of use.
    *)
    include Applicative.S

    (** Fails if it uses a field that is missing from the state. *)
    val field       : 'a Field.t    -> 'a t

    (** Evaluates to None if the field is missing from the state. Never fails. *)
    val maybe_field : 'a Field.t    -> 'a option t

    val variable    : 'a Variable.t -> 'a t

    module Args : Applicative.Args with type 'a arg := 'a t
  end

  (** An LTL formula. Evaluates to true or false given a sequence of states and
      an assignment of variables (written [xs |= t, values]). Our solver finds all
      satisfying assignments.

      Some formulas are not defined on empty sequences. Validation will reject
      such formulas along with formulas in which an argument of a [next]
      operator is not defined on empty sequences - see [validate] below for
      details. *)
  type t [@@deriving sexp_of]

  (*-----------------
    Primitives
  -------------------*)

  val const : bool -> t

  (** Undefined on empty sequences.

     [x :: _ |= (eq v fld), values] if the state [x] contains field [fld] and
     [State.get fld x = Assignment.find_exn values v].

     In particular, the negation of this formula is true if a field does not exist. *)
  val eq : 'a Variable.t -> 'a Field.t -> t

  (** Undefined on empty sequences.

      [x :: _ |= (predicate p), values] if [Expression.eval p x values] evaluates
      to [true] (the function [Expression.eval] is not exposed in this mli).

      Example:
      {[
        let gt var fld =
          predicate Expression.(map2 (variable var) (field fld) ~f:(>))
      ]}

      If expression evaluation produces failure (refers to a missing field),
      its value is considered false.
  *)
  val predicate : ?description:string -> bool Expression.t -> t

  val negate : t -> t
  val conj : t list -> t
  val disj : t list -> t

  (** [next a] means "the tail of the sequence satisfies [a]". [a] must be
      defined on empty sequences.

      [next a] is false if the sequence is empty.

      [x :: xs |= (next a), values] if [xs |= a, values].

      Because of behaviour on empty sequences [next (negate a)] is not the same
      as [negate (next a)]. *)
  val next : t -> t

  (** [until a b] means "b eventually happens, and a holds at all times until
      that (non-inclusive)".

      False on an empty sequence.

      [x :: xs |= (until a b), values] if
       [x :: xs |= b, values] or
      ([x :: xs |= a, values] and [xs |= (until a b), values])

      You almost always want to use the special case [eventually] (see
      below).  *)
  val until : t -> t -> t

  (**
     [release a b = negate (until (negate a) (negate b))] means
     "b must hold until a happens (inclusive); if a never happens, b must hold forever."

     True on an empty sequence.

     [x :: xs |= (release a b), values] if
      [x :: xs |= b, values] and
     ([x :: xs |= a, values] or [xs |= (release a b), values])

     You almost always want to use the special case [always] (see below).  *)
  val release : t -> t -> t

  (*---------------------
    Derived constructors
  -----------------------*)

  (** [before t] is true if the state has time less or equal to [t]. [before t]
      is false if the time is [None]. *)
  val before : Time_ns.t -> t

  (** [after t] is true if the state has time greater or equal to [t]. [after t]
      is false if the time is [None]. *)
  val after  : Time_ns.t -> t

  (** [before_var ~add v] is true if the state has time less or equal to
      [v + add]. *)
  val before_var : ?add : Time_ns.Span.t -> Time_ns.t Variable.t -> t

  (** [after_var ~add v] is true if the state has time greater or equal to
      [v + add]. *)
  val after_var  : ?add : Time_ns.Span.t -> Time_ns.t Variable.t -> t

  (** [field_predicate fld f] is a predicate that is true if the state contains
      [fld] and the value of [fld] satisfies [f]:

      [field_predicate fld f] = [predicate Expression.(map ~f (field fld))]
  *)
  val field_predicate
    :  ?description:string
    -> 'a Field.t
    -> ('a -> bool)
    -> t

  (* A predicate that is true if the state contains the field. *)
  val has : 'a Field.t -> t

  (* [implies a b = disj [negate a; b]] *)
  val implies : t -> t -> t

  (* [eventually] = [until (const true)] *)
  val eventually : t -> t
  (* [always] = [release (const false)] *)
  val always     : t -> t

  (* Convenient names of the operators above that shadow the pervasive ones. *)
  module O : sig
    val not : t -> t
    val (&&) : t -> t -> t
    val (||) : t -> t -> t
    val (==) : 'a Variable.t -> 'a Field.t -> t
    val (==>) : t -> t -> t
  end

  (*---------------------
    Solving
  -----------------------*)

  (** If [validate] returns [Ok] then so do [eval] and [query]. A formula may be
      rejected for several reasons:

      1 Its value is not defined for empty sequences. This is the case for
        [predicate] and [eq] formulas that refer to the initial state of the
        sequence. Validation requires that both in the top-level formula and in
        the argument of each [next] operator every [predicate] or [eq] occurs
        underneath an [until] or [release].

      2 The solver cannot reorder the clauses such that each variable is
        constrained by an equality before being used in an inequality
        or a predicate. A subformula [x != 12] is rejected, but a subformula
        [(x != 12) /\ (x == 12)] is accepted. See the top of the .ml file for
        the description of the solving algorithm that goes into more detail.

      Condition 2 guarantees that the set of all variable assignments that
      satisfy the formula is explicit: it can be described as
      [ complete(E1) U ... U complete(En) ]
      where [E1, ..., En] are variable assignments (possibly undefined for some
      variables of the formula) and [complete(E)] is the set of all assignments
      that refine [E]. This representation is returned by [query] below. *)
  val validate : t -> unit Or_error.t

  (** [eval t input] returns [true] if there exists an assignment [E] such that
      [input |= t, E].
  *)
  val eval
    :  ?debug : Log.t
    (* The implementation does not rely on time monotonicity for
       correctness. However, on a nonmonotonic sequence a formula is likely to
       not have the intuitive meaning you expect. Therefore you need to
       explicitly acknowledge if you want to deal with such sequences. *)
    -> ?allow_nonmonotonic_times : bool (* default = false *)
    -> ?allow_missing_times : bool (* default = false *)
    -> t
    -> State.t Pipe.Reader.t
    -> bool Deferred.t Or_error.t

  (* [query t input] returns a pipe of all assignments [E] such that
     [input |= t, E]. A returned assignment may be incomplete (undefined for some
     variables of [t]). Such an assignment [E] means that [t] is satisfied with
     respect to any [E'] that refines [E]. *)
  val query
    :  ?debug : Log.t
    -> ?allow_nonmonotonic_times : bool (* default = false *)
    -> ?allow_missing_times : bool (* default = false *)
    -> t
    -> State.t Pipe.Reader.t
    -> Assignment.t Pipe.Reader.t Or_error.t
end
