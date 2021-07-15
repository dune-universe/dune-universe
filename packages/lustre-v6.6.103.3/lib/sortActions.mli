(** Time-stamp: <modified the 15/01/2015 (at 10:47) by Erwan Jahier> *)



(** Topological sort of actions. Raises an error in a Dependency Cycle
   is found (hence the Lxm.t). *)

val f : Action.t list -> ActionsDeps.t -> Lxm.t -> Soc.gao list
