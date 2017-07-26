open! Core
open Core_profiler

(** An [Id_table.t] exploits the fact that [Probe_id.t]s should be consecutive integers
    to make lookups fast *)
(* Perhaps this would be far simpler if we used separate Id spaces for each type
   (with a couple of type bits for the header?) *)

type ('a, +'rw) t

(** To create a table you need to specify which [Probe_id.t]s are valid cells, and give an empty
    value.
    This can either be in the form of a list of [Probe_id.t]s, or by copying the list from
    another [Id_table.t] (whose cell contents are ignored) *)
val create : Probe_id.t list -> 'a -> ('a, _) t
val create' : (_, _) t -> 'a -> ('a, _) t

val init : Probe_id.t list -> f:(Probe_id.t -> 'a) -> ('a, _) t
val init_from_map :
  'b Probe_id.Map.t
  -> f:(Probe_id.t -> 'b -> 'a)
  -> ('a, _) t

val set_exn : ('a, read_write) t -> Probe_id.t -> 'a -> unit

(** A [('a, read_only) Id_table.t] is meant to look like an [Probe_id.Map.t]: *)
val find : ('a, _) t -> Probe_id.t -> 'a option
val find_exn : ('a, _) t -> Probe_id.t -> 'a

val iter : ('a, _) t -> f:(Probe_id.t -> 'a -> unit) -> unit

val fold :
  ('a, _) t
  -> init:'accum
  -> f:('accum -> Probe_id.t -> 'a -> 'accum)
  -> 'accum

val fold_right :
  ('a, _) t
  -> init:'accum
  -> f:('accum -> Probe_id.t -> 'a -> 'accum)
  -> 'accum

val to_alist : ('a, _) t -> (Probe_id.t * 'a) list

val map : ('a, _) t -> f:(Probe_id.t -> 'a -> 'b) -> ('b, _) t
val filter_map : ('a, _) t -> f:(Probe_id.t -> 'a -> 'b option) -> ('b, _) t

val read_only : ('a, _) t -> ('a, read) t
