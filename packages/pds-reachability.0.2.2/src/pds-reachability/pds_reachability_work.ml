open Pds_reachability_basis;;
open Pds_reachability_types;;
open Pds_reachability_utils;;

(** This module specifies the type used to describe work in a PDS reachability
    analysis as well as the interface for managing a pending work collection. *)
module type Work_type =
sig
  (** The basis module for the PDS reachability analysis. *)
  module B : Basis;;

  (** The types module for the PDS reachability analysis. *)
  module T : Types
    with module State = B.State
     and module Stack_element = B.Stack_element;;

  type t =
    | Expand_node of T.Node.t
    | Introduce_edge of T.Edge.t
    | Introduce_untargeted_dynamic_pop of
        T.Node.t * T.Untargeted_dynamic_pop_action.t
  ;;
  include Decorated_type with type t := t;;
end;;

module Make
    (B : Basis)
    (T : Types with module State = B.State
                and module Stack_element = B.Stack_element)
  : Work_type with module B = B
               and module T = T
=
struct
  module B = B;;
  module T = T;;
  type t =
    | Expand_node of T.Node.t
    | Introduce_edge of T.Edge.t
    | Introduce_untargeted_dynamic_pop of
        T.Node.t * T.Untargeted_dynamic_pop_action.t
    [@@deriving eq, ord, show, to_yojson]
  ;;
end;;
