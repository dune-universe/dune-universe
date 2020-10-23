type error =
  | Invalid_time_point_expr
  | Invalid_time_slot_expr

type f_resolve_tse_name = string -> Time_expr_ast.time_slot_expr option

type f_resolve_tpe_name = string -> Time_expr_ast.time_point_expr option

type lang_fragment =
  [ `Time_point_expr
  | `Time_slot_expr
  | `Branching_time_slot_expr
  | `Time_pattern
  ]

val check_time_expr : Time_expr_ast.t -> (unit, unit) result

module To_string : sig
  val debug_string_of_hms_ranges : Time_expr_ast.hms_expr -> string
end

val time_expr_parser :
  ?enabled_fragments:lang_fragment list -> (Time_expr_ast.t, unit) MParser.t

val of_string :
  ?enabled_fragments:lang_fragment list ->
  string ->
  (Time_expr_ast.t, string) result

val matching_time_slots :
  ?f_resolve_tpe_name:f_resolve_tpe_name ->
  ?f_resolve_tse_name:f_resolve_tse_name ->
  Search_param.t ->
  Time_expr_ast.t ->
  (Time_slot.t Seq.t, string) result

val next_match_time_slot :
  ?f_resolve_tpe_name:f_resolve_tpe_name ->
  ?f_resolve_tse_name:f_resolve_tse_name ->
  Search_param.t ->
  Time_expr_ast.t ->
  (Time_slot.t option, string) result
