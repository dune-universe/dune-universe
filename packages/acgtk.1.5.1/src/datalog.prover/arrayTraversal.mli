(** This module implements a depth-first array traversal. It is
    depth-first in order to fit with backtracking when cells contain
    persistent array. *)


module type Evaluator_TYPE =
sig
  type state
  type cell
  val cell_compare : cell -> cell -> int
  val update: state -> cell -> state option
end
  
module Make (E:Evaluator_TYPE) :
sig
  (** The type of a row *)
  type row = E.cell list
    
  (** The type of the array *)
  type array = row list
    
  (** [collect_results f acc init a] returns [f (... (f (f (f acc s1)
      s2) s3)... ) sN] where [s1 ... aN] are the states when reaching
      the end of the paths from top to bottom of [a] such that for all
      [s] resulting from the path [c1 ; ... ; cK] (all paths have this
      shape) [s = E.update (... E.update (E.update (E.update init c1)
      c2) ...) cK] and none of this [E.update] calls returned a [None]
      value (hence the notation abuse of applying [E.update] to a
      [state] instead of a [state option] in this description).*)
  val collect_results : ('a -> E.state -> 'a) -> 'a -> E.state -> array -> 'a
end

module type Evaluator_TYPE2 =
sig
  type state
  type cell
  module CellSet:Set.S with type elt= cell
  val update: state -> cell -> state option
end


module Make2 (E:Evaluator_TYPE2) :
sig
  (** The type of a row *)
  type row = E.CellSet.t
    
  (** The type of the array *)
  type array = row list
    
  (** [collect_results f acc init a] returns [f (... (f (f (f acc s1)
      s2) s3)... ) sN] where [s1 ... aN] are the states when reaching
      the end of the paths from top to bottom of [a] such that for all
      [s] resulting from the path [c1 ; ... ; cK] (all paths have this
      shape) [s = E.update (... E.update (E.update (E.update init c1)
      c2) ...) cK] and none of this [E.update] calls returned a [None]
      value (hence the notation abuse of applying [E.update] to a
      [state] instead of a [state option] in this description).*)
  val collect_results : ('a -> E.state -> 'a) -> 'a -> E.state -> array -> 'a
end
