type range = Lexing.position * Lexing.position
exception Unbound of range * string
exception Unify of range * F.nominal_type * F.nominal_type
exception Cycle of range * F.nominal_type

val translate: ML.term -> F.nominal_term
