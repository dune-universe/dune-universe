(** Return a hash table containing the set of available fonts with their
    full XLFD description.
    Calling [get] implies a complete look up of fonts by the X server.
    Hence, it may need some time to complete.
 *)

val get : unit -> Xfonts.x_fonts_table;;
val get_fresh : unit -> Xfonts.x_fonts_table;;
val set_cache_file_name : Xfonts.file_name -> unit;;
val get_cache_file_name : unit -> Xfonts.file_name;;
