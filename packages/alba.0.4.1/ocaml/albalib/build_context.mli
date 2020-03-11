type t

type type_in_context = int list * Term.typ * Gamma.t


(**

A build context consists of a context with holes and a stack of to be
constructed terms.

There is always a next to be constructed term. The term is either in a function
position or an argument position.

*)


val count: t -> int

val count_base: t -> int

val count_locals: t -> int

val count_bounds: t -> int

val required_type_in_context: t -> type_in_context

val make: Gamma.t -> t
(**
    [make gamma]

    Make a build context based on [gamma]. Push 2 holes onto the context to get

    Gamma, E: Any(2), e: E

    The next to be constructed term points to [e].
*)


val final:
        t
        -> (Term.t * Term.typ, int list * Term.typ * Gamma.t) result


(** {1 Terminals } *)



val base_candidate: Term.t -> int -> t -> t option
(**
    [base_candidate term nargs bc]

    Receive the term [term] as a candidate for the next to be constructed term.
    The candidate is from the base context and in applied to [nargs] arguments.

    Base candidates are either

    - Literals (Numbers, characters, strings)

    - Variables from the base context
*)


val bound: int -> int -> t -> (t, type_in_context * type_in_context) result
(**
    [bound level nargs bc]
*)



(*
val candidate: Term.t -> t -> t option
(**
    [candidate term bc]

    Receive the term [term] as a candidate for the next to be constructed term.

    Candidates are either

    - Base candidates

    - Bound variables


New placeholder for implicit arguments have to be generated in case that the
required type is not just a placeholder. The candidate term has to be applied to
the implicit arguments before assigning it to the next to be constructed term.

*)
*)




(** {1 Product [all (a: A) ... : RT]} *)

module Product:
sig
    val start: t -> t
    val next: string -> bool -> t -> t
    val end_: int -> t -> (t, int) result
end


(** {1 Typed expression [exp: tp]} *)

module Typed:
sig
    val start: t -> t
    val expression: t -> t
    val end_: int -> t -> (t, type_in_context * type_in_context) result
end


(** {1 Function Application [f a b c ... ]} *)

module Application:
sig
    val start:  int -> t -> t
    (** [start nargs bc] Start a function application with [nargs] arguments. *)

    val apply:
        int
        -> Term.Application_info.t
        -> t
        -> (t, type_in_context * type_in_context) result
    (** [apply n_remaining mode bc] Apply the function to the argument. There are
    [n_remaining] remaining arguments. *)
end


(** {1 Function Abstraction [\ x y ... := t]} *)

module Lambda:
sig
    val start: t -> t
    (** Start a function abstraction and expect the first argument type. *)


    val next:  string -> bool -> t -> t
    (** [next name typed bc]

        Add a bound variable whose type is the last analyzed expression and
        expect the next variable type or the type of the inner expression.
    *)


    val inner: t -> t
    (** Expect the inner expression of the function abstraction whose type is
    the last analyzed expression. *)


    val end_:
        int -> int -> bool -> t
        -> (t, type_in_context * type_in_context) result
    (**
        [end_ nargs nbounds typed cb]

    End the function abstraction with [nbounds] bound variables. The function
    abstraction is applied to [nargs] arguments.
    *)
end
