(** Return a hash table containing the set of available fonts with their
    full XLFD description.
    The [get] function is smart enough to store its result into a file.
    Hence, unless nobody has ever called it, [get] is fast. *)

val get : unit -> Xfonts.x_fonts_table;;

val get_fresh : unit -> Xfonts.x_fonts_table;;
(** Same as above, but remove the disk file cache and returns a freshly computed table. *)

val set_cache_file_name : Xfonts.file_name -> unit;;
val get_cache_file_name : unit -> Xfonts.file_name;;
(** To set and get the name of the cache file for the computed table. *)
