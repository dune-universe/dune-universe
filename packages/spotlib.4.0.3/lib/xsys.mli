val mkdir : ?perm:Unix.file_perm -> ?recursive:bool -> string ->
  (unit, string * [> `Already_exists of Unix.stats
                  | `Not_a_directory of Unix.stats
                  | `Unix of Unix.error ]) result
  (** Create a directory of the given name. Does nothing if the
      directory exists already. 

      Bug: No chmod if the directroy already exists.
  *)
      
val checkenv : string -> bool
  (** check existence of environment variable *)

val command : ('a, unit, string, int) format4 -> 'a
val must : ('a, unit, string, unit) format4 -> 'a
val cp : string -> string -> unit
val patch_p1 : string -> unit

val with_chdir : string -> ('a -> 'b) -> 'a -> 'b
(** may raise [Invalid_arg "Xsys.with_chdir"] *)

val with_chdir_ : string -> (unit -> 'b) -> 'b
(** [with_chdir_ d f = with_chdir d f ()] *)
