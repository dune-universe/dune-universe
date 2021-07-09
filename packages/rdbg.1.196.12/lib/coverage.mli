

type t 

(* [init var_names cov_file reinit_cov] *)
val init : string list -> string -> bool -> t 
val get_file_name : t -> string

val compute_stat :  t -> int * int * float 
val update_cov : Data.subst list -> t ->  t

(* Returns a synthesis of the oracle state which intend to be useful
   for users when the oracle returns false.  *)
val dump_oracle_io : Data.subst list -> Data.subst list -> t -> string


(* [dump ecfile riffile cov] *)
val dump : string -> string -> t -> unit
