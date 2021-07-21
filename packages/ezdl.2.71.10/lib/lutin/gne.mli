(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: gne.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Garded normal expressions.

They are Ne.t expressions garded by formula (a Bdd.t actually).  The
interpretation of [(e1 -> bdd1 ; ... ; en -> bddn)] is that it is
equal to [ei] iff [bddi] is true. By construction, the set [{bddi,
i=1,n}] ougth to be a partition (namely, exactly one bdd among the
[bddi] is true).
*)

type t


(** Respectively adds, substracts, multiplies, divides, and computes
  the modulo, the opposite of garded normal expressions.
*)
val add :    t -> t -> t
val diff :   t -> t -> t
val mult :   t -> t -> t
val modulo : t -> t -> t
val div :    t -> t -> t
val opposite : t -> t

(* The same as div, except that is raises an internal error if 
   the second arg does not divide the first.

   For internal use only.
*)
val quot :   t -> t -> t


(** [empty ()] returns an empty gne.t *)
val empty : unit -> t

(** [make ne bdd b] returns the gne containing the expr [ne] guarded
  by [(bdd_true, b)]. *)
val make : Ne.t -> bool -> t

(** [of_ite cond flag gne_then gne_else]    *)
val of_ite : Bdd.t -> bool -> t -> t -> t


(** [fold f gne] applies [f] to every expr in [gne]. *)
val fold : (Ne.t -> Bdd.t * bool -> 'acc -> 'acc) -> t -> 'acc -> 'acc


(** [get_constant gne returns an Value.num  iff gne is equal to
  the degenerated garded normal expression [(i, true)]. This function
  is useful to get dynamic weigths and default values. *)
val get_constant : t -> Value.num option

(** Pretty-printing *)
val to_string : t -> string
val dump : t -> unit
