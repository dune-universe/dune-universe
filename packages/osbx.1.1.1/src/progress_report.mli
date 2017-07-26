val seconds_to_hms            : int -> int * int * int

val gen_print_generic         : header:string -> unit:string -> print_interval:float -> (start_time:float -> units_so_far:int64 -> total_units:int64 -> unit)

val print_newline_if_not_done : units_so_far:int64 -> total_units:int64 -> unit
