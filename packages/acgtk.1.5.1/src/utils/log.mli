

val stamp : Mtime_clock.counter -> Logs.Tag.set

(** [set_level ~app l] sets the log level to [l] and provides the name
    [app] to the default application log. *)
val set_level : app:string -> ?colored:bool -> Logs.level -> unit


