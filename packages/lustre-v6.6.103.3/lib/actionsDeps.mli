(** Time-stamp: <modified the 11/07/2017 (at 14:37) by Erwan Jahier> *)

(** Compute dependencies between actions  *)


type t

val empty : t

(** Linear in the size of the first parameter *)
val concat: t -> t -> t


(** Compute the action dependencies that comes from the equations I/O. 

    Construit des dépendances entre les actions en reliant les entrées et
    les sorties de ces actions. 

    Lic2soc.lic_to_soc_type is passed in argument to break a dep loop
*)
val build_data_deps_from_actions:  (Lic.type_ -> Data.t) -> t -> Action.t list -> t

(** Use the dependency constraints that come from the SOC (e.g., 'get' before 'set'
    in memory SOC).
*)
val generate_deps_from_step_policy: Soc.precedence list -> (string * Action.t) list -> t

(** Returns the list of actions that depends on the action in argument. *)
val find_deps: t -> Action.t -> Action.t list
val have_deps : t -> Action.t -> bool
val remove_dep :  t -> Action.t -> t

(* Could also be named is_greater because if a1 depends on a2, it
   means that a2 should be computed before, i.e. that a2 is smaller than
   a1 *)
val depends_on : t -> Action.t -> Action.t -> bool

val to_string: t -> string

