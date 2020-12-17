type name_violation =
    | Upper_for_proposition
    | Lower_for_type
    | Upper_for_object


val strings_of_violation: name_violation -> string * string



module type GAMMA =
sig
    type t

    val count: t -> int
    (** Number of variables in the context. *)


    val is_valid_index: int -> t -> bool
    val name_of_index: int -> t -> string

    val push_local: string -> Term.typ -> t -> t
    (**
        [push_local name typ gamma]

        Add the local variable with [name] and [typ] to the context.

        Precondition
        {[name <> ""]}

    *)

    val type_of_literal: Value.t -> t -> Term.typ


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

    val split_type:
        Term.typ -> Gamma.t
        ->  (Term.Pi_info.t * Term.typ) list * Term.typ


    val split_kind:
        Term.typ
        -> Gamma.t
        -> ((Term.Pi_info.t * Term.typ) list * Sort.t) option
    (**
        [split_kind k gamma]:

        Compute the arguments and the sort of the kind [k]. If [typ] does not
        reduce to a kind, then return [None].

        Precondition: [k] must be welltyped and the context [gamma] must be
        valid.

        A kind has the form

        {[all (x: A) (y: B) .... : s]}

        where [s] is a sort.
    *)





    val sort_of_kind: Term.typ -> Gamma.t -> Sort.t option
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



    val key_split:
        Term.t
        -> Gamma.t
        -> Term.t * (Term.t * Term.Application_info.t) list



    val key_normal: Term.t -> Gamma.t -> Term.t
    (**
        [keynormal term gamma]

        Compute the key normal form of the welltyped term [term] in the valid
        context [gamma].
    *)


    val normalize_pi: Term.typ -> Gamma.t -> Term.typ
    (** [normalize_pi typ gamma]

        Precondition:

        [typ] must be a valid type

        Result: [typ] is expanded until the form
        {[
            all (a: A) (b: B) ... : R
        ]}

        is reached where [R] cannot be expanded further into a product type.

    *)


    val normalize: Term.t -> Gamma.t -> Term.t
    (**
        [normalize term gamma]

        Compute the normal form of the welltyped term [term] in the valid
        context [gamma].
    *)

    val check_naming_convention:
        string -> Term.typ -> Gamma.t -> (unit, name_violation) result
end
