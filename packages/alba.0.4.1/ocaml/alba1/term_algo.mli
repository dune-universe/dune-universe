open Term

val unify_pattern: int -> term -> int -> term -> term array


val compare: term -> term -> (term->term->'a)
  -> term * 'a array * term array * term array
