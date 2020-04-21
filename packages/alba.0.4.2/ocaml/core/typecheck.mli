val is_valid_context: Gamma.t -> bool
(**
    [is_valid_context gamma]

    Is the context [gamma] wellformed?
*)

val check: Term.t -> Gamma.t -> Term.typ option
(**
    [check term gamma]

    Verify if [term] is welltyped in the valid context [gamma]. If yes,
    return its type.

    Precondition:
    {[is_valid_context gamma]}
*)

