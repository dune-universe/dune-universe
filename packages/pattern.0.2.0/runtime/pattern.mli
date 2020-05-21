include module type of struct
  include Types
end

val check :
    ('a -> Parsetree.expression) -> 'a -> ('a, 'b) matcher ->
      'b pattern_result

val format_failure : Format.formatter -> failure -> unit

val elim_type_constraints : Parsetree.expression -> Parsetree.expression

val pp_failure : Format.formatter -> failure -> unit

val pp_pattern_result :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern_result ->
      unit
