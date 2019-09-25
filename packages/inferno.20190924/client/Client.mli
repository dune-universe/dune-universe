exception Unbound of string
exception Unify of F.nominal_type * F.nominal_type
exception Cycle of F.nominal_type

val translate: ML.term -> F.nominal_term
