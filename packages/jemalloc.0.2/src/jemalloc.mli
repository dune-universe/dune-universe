(**
  Bindings to jemalloc. Monitor memory usage and control behaviour of jemalloc.
*)

(** {2 malloc interface} *)

(** Release as much memory as possible to operating system *)
val release_free_memory : unit -> unit

exception Invalid_property of string

(** @return version information: (human-readable string, major, minor, git version) *)
val version : unit -> string * int * int * string


val mallctl_bool : string -> bool option -> bool
val mallctl_int : string -> int option -> int
val mallctl_string : string -> string option -> string
val mallctl_unit : string -> unit

type memory_stats = {
  active: int;
  resident: int;
  allocated: int;
  mapped: int;
}

(** epoch counter for the stats. Statistics are frozen to a snapshot value until the next [epoch] call refresh the snapshot *)
val epoch : unit -> unit

(** @return stat counter from the "stats." mallctl prefix during current epoch *)
val get_stat : string -> int

(** refresh epoch and gather memory statistics  @return [memory_stats] *)
val get_memory_stats : unit -> memory_stats
