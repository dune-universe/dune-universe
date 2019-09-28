include module type of struct
  include Types
end

val check :
    ('a -> Ppxlib.expression) -> 'a -> ('a, 'b) matcher ->
      'b pattern_result

val format_failure : Format.formatter -> failure -> unit

val elim_type_constraints : Ppxlib.expression -> Ppxlib.expression
