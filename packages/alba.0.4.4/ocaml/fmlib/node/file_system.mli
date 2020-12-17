val mkdir: string -> (unit option -> unit) -> unit
val rmdir: string -> (unit option -> unit) -> unit

val readdir: string -> (string array option -> unit) -> unit

val stat: string -> 'a option

val open_:
    string
    -> string
    -> ((int, Fmlib.Io.Error.t) result -> unit)
    -> unit

val close: int -> ((unit, Fmlib.Io.Error.t) result -> unit) -> unit

val read:
    int
    -> Io_buffer.t
    -> ((int, Fmlib.Io.Error.t) result -> unit)
    -> unit


val write: int -> Io_buffer.t -> (int -> unit) -> unit
