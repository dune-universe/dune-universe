val set_verbose : int -> unit
val verbose : unit -> int

val status : unit -> string
val set_status : string -> unit

val quiet : unit -> bool
val set_quiet : bool -> unit

val assert_mode : unit -> bool
val set_assert_mode : bool -> unit

val keep_loc : unit -> bool
val set_keep_loc : bool -> unit

val warning_as_error : unit -> bool
val set_warning_as_error : bool -> unit

val filename : unit -> string
val set_filename : string -> unit

val set_logic : bool -> unit
val set_is_qf : bool -> unit
val set_is_uf : bool -> unit
val set_is_fp : bool -> unit
val set_is_real : bool -> unit
val set_is_int_real : bool -> unit
val set_is_dt : bool -> unit
val set_is_linear : bool -> unit
val set_is_non_linear : bool -> unit

val get_logic : unit -> bool
val get_is_qf : unit -> bool
val get_is_uf : unit -> bool
val get_is_fp : unit -> bool
val get_is_real : unit -> bool
val get_is_int_real : unit -> bool
val get_is_dt : unit -> bool
val get_is_linear : unit -> bool
val get_is_non_linear : unit -> bool

val set_err_fmt : Format.formatter -> unit
val get_err_fmt : unit -> Format.formatter

val check_command : string -> unit
