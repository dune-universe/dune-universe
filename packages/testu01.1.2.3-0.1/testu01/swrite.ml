external get_basic : unit -> bool = "caml_swrite_getBasic"
external set_basic : bool -> unit = "caml_swrite_setBasic"

external get_parameters : unit -> bool = "caml_swrite_getParameters"
external set_parameters : bool -> unit = "caml_swrite_setParameters"

external get_collectors : unit -> bool = "caml_swrite_getCollectors"
external set_collectors : bool -> unit = "caml_swrite_setCollectors"

external get_classes : unit -> bool = "caml_swrite_getClasses"
external set_classes : bool -> unit = "caml_swrite_setClasses"

external get_counters : unit -> bool = "caml_swrite_getCounters"
external set_counters : bool -> unit = "caml_swrite_setCounters"

external get_host : unit -> bool = "caml_swrite_getHost"
external set_host : bool -> unit = "caml_swrite_setHost"
