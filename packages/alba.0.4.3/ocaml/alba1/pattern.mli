open Term

type match_result = (term array * term list, term list) result

val is_pattern: int -> term -> int -> Feature_table.t -> bool

val unify_with_pattern:
  term -> int -> term -> Context.t -> match_result option

val decide_inspect: term -> (formals*term*term) array -> Context.t
                    -> (int * arguments * term list) option

val evaluated_as_expression: term -> Context.t -> term
