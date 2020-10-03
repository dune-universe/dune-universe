include module type of struct
  include Types
end

val check :
    ('a -> Ppxlib.expression) -> 'a -> ('a, 'b) matcher ->
      'b pattern_result

val format_failure : Format.formatter -> failure -> unit

val elim_type_constraints : Ppxlib.expression -> Ppxlib.expression

val pp_failure : Format.formatter -> failure -> unit

val pp_pattern_result :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern_result ->
      unit
