open Fmlib
open Alba_core

type pos = Character_parser.Position.t
type range = pos * pos

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

val count_entries: t -> int

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
        -> (t * Term.t * Term.typ, int list * Term.typ * Gamma.t) result


(** {1 Terminals } *)



val base_candidate: range -> int -> Term.t -> int -> t -> t option
(**
    [base_candidate range variant term nargs bc]

    Receive the term [term] as a candidate for the next to be constructed term.
    The candidate is from the base context and in applied to [nargs] arguments.

    Base candidates are either

    - Literals (Numbers, characters, strings)

    - Variables from the base context
*)


val find_last_ambiguous: t list -> range * (Term.t * Term.typ) list

val bound: int -> int -> t -> (t, type_in_context * type_in_context) result
(**
    [bound level nargs bc]
*)





val next_formal_argument: string Character_parser.Located.t -> bool -> t -> t
(** Add a bound variable based on the last argument type and push a
placeholder for the next argument type or the result type. I.e. expect the
next argument type or the result type. *)


val find_first_untyped_formal: t -> range option



val find_first_name_violation:
    t -> (range * string * string) option


(** {1 Product [all (a: A) ... : RT]} *)

module Product:
sig
    val start: t -> t

    val check: int -> t -> (t, int) result

    val end_:
        int -> int -> t
        -> (t, type_in_context * type_in_context) result
    (**
        [end_ nargs nbounds bc]
    *)
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






(** {1 Where expression [exp where f ... := value]}

The where expression

{[
    exp where
        f := value
]}

is treated like

{[
    (\f := exp) value
]}

i.e.
{[
    Appl ( Lambda (f, F, exp), value)
]}

i.e. a beta redex, and finally converted to

{[
    Where (f, F, exp, value)
]}

*)

module Where:
sig
    val start: string Character_parser.Located.t -> t -> t
    (** Start a where expression with the name of the local definition. *)

    val end_inner: t -> (t, type_in_context * type_in_context) result

    val end_: int -> t -> (t, type_in_context * type_in_context) result
end
