(**
   This module defines basic data types for the PDS reachability implementation
   and utility functions for them.
*)
open Batteries;;
open Pds_reachability_types_stack;;
open Pds_reachability_utils;;

module type Types =
sig
  (** The decorated type of states in the PDS. *)
  module State : Decorated_type

  (** The decorated type of stack elements in the PDS. *)
  module Stack_element : Decorated_type

  (** The decorated type of targeted dynamic pop actions in the PDS. *)
  module Targeted_dynamic_pop_action : Decorated_type

  (** The decorated type of untargeted dynamic pop actions in the PDS. *)
  module Untargeted_dynamic_pop_action : Decorated_type

  (** Stack actions which may be performed in the PDS. *)
  module Stack_action : Decorated_stack_action_type
    with type stack_element = Stack_element.t
     and type targeted_dynamic_pop_action = Targeted_dynamic_pop_action.t

  (** Termini of paths which may exist in the PDS. *)
  module Terminus : Decorated_terminus_type
    with type state = State.t
     and type untargeted_dynamic_pop_action = Untargeted_dynamic_pop_action.t

  type intermediate_destination =
    | Static_destination of node
    | Dynamic_destination of Untargeted_dynamic_pop_action.t

  and node =
    | State_node of State.t
    | Intermediate_node of intermediate_destination * Stack_action.t list

  (** The decorated type of node used for reachability. *)
  module Node : Decorated_type with type t = node

  type edge =
    { source : Node.t
    ; target : Node.t
    ; edge_action : Stack_action.t
    };;

  (** The decorated type of edge used in reachability. *)
  module Edge : Decorated_type with type t = edge
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State
    )
  : Types with module Stack_element = Basis.Stack_element
           and module State = Basis.State
           and module Targeted_dynamic_pop_action =
                 Dph.Targeted_dynamic_pop_action
           and module Untargeted_dynamic_pop_action =
                 Dph.Untargeted_dynamic_pop_action
           and module Stack_action = Dph.Stack_action
           and module Terminus = Dph.Terminus
=
struct
  module State = Basis.State;;
  module Stack_element = Basis.Stack_element;;
  module Targeted_dynamic_pop_action = Dph.Targeted_dynamic_pop_action;;
  module Untargeted_dynamic_pop_action = Dph.Untargeted_dynamic_pop_action;;
  module Stack_action = Dph.Stack_action;;
  module Terminus = Dph.Terminus;;

  type intermediate_destination =
    | Static_destination of node
    | Dynamic_destination of Untargeted_dynamic_pop_action.t
  [@@deriving eq, ord, show, to_yojson]

  and node =
    | State_node of State.t
    | Intermediate_node of intermediate_destination * Stack_action.t list
  [@@deriving eq, ord, show, to_yojson]
  ;;
  
  let _ = show_intermediate_destination;;

  module Node =
  struct
    type t = node
    let equal = equal_node
    let compare = compare_node
    let pp = pp_node
    let show = show_node
    let to_yojson = node_to_yojson
  end;;

  type edge =
    { source : node
    ; target : node
    ; edge_action : Stack_action.t
    }
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Edge =
  struct
    type t = edge
    let equal = equal_edge
    let compare = compare_edge
    let pp = pp_edge
    let show = show_edge
    let to_yojson = edge_to_yojson
  end;;
end;;
