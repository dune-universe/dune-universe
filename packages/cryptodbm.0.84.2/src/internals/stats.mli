
(* Statistics about keys and data. *)
type t

(* Creates a new statistic variable. *)
val new_stats : unit -> t

(* Adds a pair (key, data) in the statistics. *)
val put : t -> Kinds.encoded_key -> Kinds.encoded_data -> unit

(* Given an insertion function, inserts random bindings looking like the given statistics. 
 * The second argument is max_extra_bindings (in percent). *)
val insert : t -> int -> (key:string -> data:string -> unit) -> unit


