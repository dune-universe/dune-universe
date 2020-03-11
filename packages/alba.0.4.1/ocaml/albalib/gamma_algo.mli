module type GAMMA =
sig
    type t

    val count: t -> int
    (** Number of variables in the context. *)


    val push_local: string -> Term.typ -> t -> t
    (**
        [push_local name typ gamma]

        Add the local variable with [name] and [typ] to the context.

        Precondition
        {[name <> ""]}

    *)

    val type_of_literal: Term.Value.t -> t -> Term.typ


    val type_of_variable: int -> t -> Term.typ
    (**
        [type_of_variable idx gamma]

        Return the type of the variable [idx]. [idx] is the De Bruijn index i.e.
        and index of [0] points to the last variable and an index of [count
        gamma - 1] points to the first variable.
    *)

    val definition_term: int -> t -> Term.t option
    (**
        [definition_term idx gamma]

        If the variable [idx] has a definition, then return it. Otherwise return
        [None].
    *)
end





module Make (Gamma: GAMMA):
sig
    val type_of_term: Term.t -> Gamma.t -> Term.typ
    (**
        [type_of_term term gamma]

        Compute the type of [term]

        Precondition: [term] must be welltyped and the context valid.
    *)


    val sort_of_kind: Term.typ -> Gamma.t -> Term.Sort.t option
    (**
        [sort_of_kind typ gamma]

        Compute the sort of the kind [typ]. If [typ] does not reduce to a kind,
        then return [None].

        Precondition: [typ] must be welltyped and the context [gamma] must be
        valid.

        A kind has the form

        {[all (x: A) (y: B) .... : s]}

        where [s] is a sort.
    *)


    val is_kind: Term.typ -> Gamma.t -> bool
    (**
        [is_kind typ gamma]

        Is the welltyped term [typ] in the valid context [gamma] a kind?
    *)




    val key_normal: Term.t -> Gamma.t -> Term.t
    (**
        [keynormal term gamma]

        Compute the key normal form of the welltyped term [term] in the valid
        context [gamma].
    *)
end
