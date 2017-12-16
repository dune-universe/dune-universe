type engine = STRINGS | LOGIC | DERIVED_TREES | TREES | DEFAULT

type config

val get_config : string -> string list -> config

val default : config
  
val background_color : config -> int * int * int
val node_color : config -> int * int * int
val engines : config -> engine UtilsLib.Utils.StringMap.t
