module Problem:
sig
    type t =
    | Out_of_bound of int * Gamma.t
    | Argument_type
    | Typed
    | Lambda
    | No_type
    | Name of string
end

type path = int list



val is_valid_context: Gamma.t -> bool
(**
    [is_valid_context gamma]

    [gamma] is wellformed if
    - every type is valid, i.e. it typechecks using only entries of [gamma] with a smaller level (this is true by contruction, since we are using de bruijn indices)
    - the same holds for all definition terms
    - {b TODO}: every type is a type (and not an object)

    @return true, if [gamma] is wellformed, false otherwise

*)



val equivalent: Term.typ -> Term.typ -> Gamma.t -> bool




val check: Term.t -> Gamma.t -> (Term.typ, path * Problem.t) result
(**
    [check term gamma]

    Verify if [term] is welltyped in the valid context [gamma]. If yes,
    return its type.

    Precondition:
    {[is_valid_context gamma]}
*)

