open Apg_types;;

val apg_file_name : string -> file_name;;
 (** Return a valid [.apg] file name.
  if [s] is empty, the file name defaults to [a.apg],
  if [s] is ["-"], [s] is returned. *)
val save_apg : file_name -> program -> unit;;
val load_apg : file_name -> program;;
