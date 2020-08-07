type iface

val create_interface: unit -> iface

val question: iface -> string -> (string -> unit) -> (unit -> unit) -> unit

val close: iface -> unit
