type kind_parameter =
  | Type
  | Term

type type_or_term_value =
  | Type_value of Logic.Abstract_syntax.Abstract_syntax.type_def
  | Term_value of Logic.Abstract_syntax.Abstract_syntax.term
  | Term_token of Term_sequence.token
