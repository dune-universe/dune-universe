type silence_level = L0 | L1

type silence_settings

type ('a, 'b, 'c) progress_print_functions =
  { print_progress            :
      start_time_src:'a   ->
      units_so_far_src:'b ->
      total_units_src:'c  ->
      unit
  ; print_newline_if_not_done :
      start_time_src:'a   ->
      units_so_far_src:'b ->
      total_units_src:'c  ->
      unit
  }

type progress_element = Percentage
                      | Progress_bar
                      | Current_rate_short
                      | Average_rate_short
                      | Time_used_short
                      | Time_left_short
                      | Current_rate_long
                      | Average_rate_long
                      | Time_used_long
                      | Time_left_long

module Helper : sig
  val seconds_to_hms : int -> int * int * int

  val silence_level_to_silence_settings : silence_level option -> silence_settings
end

val default_silence_settings : silence_settings

val gen_print_generic :
  header                  : string ->
  silence_settings        : silence_settings ref ->
  display_while_active    : progress_element list ->
  display_on_finish       : progress_element list ->
  display_on_finish_early : progress_element list ->
  unit                    : string ->
  print_interval          : float ->
  eval_start_time         : ('a -> float) ->
  eval_units_so_far       : ('b -> int64) ->
  eval_total_units        : ('c -> int64) ->
  ('a, 'b, 'c) progress_print_functions
