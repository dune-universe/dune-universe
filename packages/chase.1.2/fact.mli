(** Facts - structure elements *)

open Sym

type fact =
  | Q of sym * sym list
  (** Predicate symbol applied to constants *)
  | G of sym * sym list * sym
  (** Function symbol applied to constants gives constant *)

(** A total ordering for facts *)
val compr : fact -> fact -> int

(** Sets of facts *)
module FactSet : Set.S with type elt = fact
