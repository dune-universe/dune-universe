val get_prelude : unit -> string

val set_prelude : string -> unit

val set_prelude_from_file : string -> unit

val get_filename_prefix : unit -> string

val set_filename_prefix : string -> unit

val set_required_files : string list -> unit

val get_required_files : unit -> File.t list

val append_required_file : string -> unit

val get_t1disasm : unit -> string option

val set_t1disasm : string option -> unit

val set_verbosity : bool -> unit

val get_verbosity : unit -> bool

val set_debug : bool -> unit

val get_debug : unit -> bool

type job = string * Types.commandpic

type jobs = job list

val figures : job Queue.t

val emit : string -> Types.commandpic -> unit

val emited : unit -> jobs
