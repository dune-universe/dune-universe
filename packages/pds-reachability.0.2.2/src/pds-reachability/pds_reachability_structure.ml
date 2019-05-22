(**
   This module defines a data structure used in a PDS reachability analysis.
*)
open Batteries;;
open Jhupllib;;
open Pds_reachability_utils;;

(**
   The type of the module which defines the data structure used within the
   analysis.
*)
module type Structure =
sig
  (** The decorated stack element type used by the PDS. *)
  module Stack_element : Decorated_type

  (** The decorated node type in the reachability structure. *)
  module Node : Decorated_type

  (** The decorated edge type in the reachability structure. *)
  module Edge : Decorated_type

  (** The decorated type of targeted dynamic pop actions in this structure. *)
  module Targeted_dynamic_pop_action : Decorated_type

  (** The decorated type of untargeted dynamic pop actions in this structure. *)
  module Untargeted_dynamic_pop_action : Decorated_type

  (** The type of the PDS reachability data structure. *)
  include Decorated_type

  (**
     Produces a Yojson structure representing the contents of a latter analysis
     which do not appear in the former.
  *)
  val to_yojson_delta : t -> t -> Yojson.Safe.json

  (** The empty PDS reachability structure. *)
  val empty : t

  (** Adds an edge to a reachability structure. *)
  val add_edge : Edge.t -> t -> t

  (** Determines if a structure has a particular edge. *)
  val has_edge : Edge.t -> t -> bool

  (** Adds an untargeted dynamic pop action to a reachability structure. *)
  val add_untargeted_dynamic_pop_action :
    Node.t -> Untargeted_dynamic_pop_action.t -> t -> t

  (** Determines if a given untargeted dynamic pop action is present in a
      reachability structure. *)
  val has_untargeted_dynamic_pop_action :
    Node.t -> Untargeted_dynamic_pop_action.t -> t -> bool

  (** {6 Query functions.} *)

  val find_push_edges_by_source
    : Node.t -> t -> (Node.t * Stack_element.t) Enum.t
  val find_pop_edges_by_source
    : Node.t -> t -> (Node.t * Stack_element.t) Enum.t
  val find_nop_edges_by_source
    : Node.t -> t -> Node.t Enum.t
  val find_targeted_dynamic_pop_edges_by_source
    : Node.t -> t -> (Node.t * Targeted_dynamic_pop_action.t) Enum.t
  val find_untargeted_dynamic_pop_actions_by_source
    : Node.t -> t -> Untargeted_dynamic_pop_action.t Enum.t
  val find_push_edges_by_target
    : Node.t -> t -> (Node.t * Stack_element.t) Enum.t
  val find_pop_edges_by_target
    : Node.t -> t -> (Node.t * Stack_element.t) Enum.t
  val find_nop_edges_by_target
    : Node.t -> t -> Node.t Enum.t

  val enumerate_nodes : t -> Node.t Enum.t
  val enumerate_edges : t -> Edge.t Enum.t

  (** {6 Submodules.} *)

  module Node_set : Set.S with type elt = Node.t
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State)
    (Types : Pds_reachability_types.Types
     with module State = Basis.State
      and module Stack_element = Basis.Stack_element
      and module Targeted_dynamic_pop_action = Dph.Targeted_dynamic_pop_action
      and module Untargeted_dynamic_pop_action = Dph.Untargeted_dynamic_pop_action
    )
  : Structure
    with module Stack_element = Basis.Stack_element
     and module Edge = Types.Edge
     and module Node = Types.Node
     and module Targeted_dynamic_pop_action = Types.Targeted_dynamic_pop_action
     and module Untargeted_dynamic_pop_action =
           Types.Untargeted_dynamic_pop_action
=
struct
  module State = Basis.State;;
  module Stack_element = Basis.Stack_element;;
  module Edge = Types.Edge;;
  module Node = Types.Node;;
  module Targeted_dynamic_pop_action = Types.Targeted_dynamic_pop_action
  module Untargeted_dynamic_pop_action =
    Types.Untargeted_dynamic_pop_action
  ;;

  open Types;;
  open Types.Stack_action.T;;

  (********** Simple internal data structures. **********)

  type node_and_stack_element =
    Node.t * Stack_element.t
  [@@deriving eq, ord, show, to_yojson]
  ;;

  type node_and_targeted_dynamic_pop_action =
    Node.t * Targeted_dynamic_pop_action.t
  [@@deriving eq, ord, show, to_yojson]
  ;;

  (********** Substructure definitions. **********)

  module Node_set = Set.Make(Node);;

  module Node_and_stack_element =
  struct
    type t = node_and_stack_element;;
    let equal = equal_node_and_stack_element;;
    let compare = compare_node_and_stack_element;;
    let pp = pp_node_and_stack_element;;
    let show = show_node_and_stack_element;;
    let to_yojson = node_and_stack_element_to_yojson;;
  end;;

  module Node_and_targeted_dynamic_pop_action =
  struct
    type t = node_and_targeted_dynamic_pop_action;;
    let equal = equal_node_and_targeted_dynamic_pop_action;;
    let compare = compare_node_and_targeted_dynamic_pop_action;;
    let pp = pp_node_and_targeted_dynamic_pop_action;;
    let show = show_node_and_targeted_dynamic_pop_action;;
    let to_yojson = node_and_targeted_dynamic_pop_action_to_yojson;;
  end;;

  module Make_nice_multimap(K : Decorated_type)(V : Decorated_type) =
  struct
    module Impl = Multimap.Make(K)(V);;
    include Impl;;
    include Multimap_pp.Make(Impl)(K)(V);;
    include Multimap_to_yojson.Make(Impl)(K)(V);;
  end;;

  module Node_to_node_and_stack_element_multimap =
    Make_nice_multimap(Node)(Node_and_stack_element)
  ;;

  module Node_and_stack_element_to_node_multimap =
    Make_nice_multimap(Node_and_stack_element)(Node)
  ;;

  module Node_to_node_and_targeted_dynamic_pop_action_multimap =
    Make_nice_multimap(Node)(Node_and_targeted_dynamic_pop_action)
  ;;

  module Node_to_untargeted_dynamic_pop_action_multimap =
    Make_nice_multimap(Node)(Untargeted_dynamic_pop_action)
  ;;

  module Node_and_targeted_dynamic_pop_action_to_node_multimap =
    Make_nice_multimap(Node_and_targeted_dynamic_pop_action)(Node)
  ;;

  module Node_to_node_multimap =
    Make_nice_multimap(Node)(Node)
  ;;

  (********** The data structure itself. **********)

  (** The type of the PDS reachability graph data structure. *)
  type t =
    { push_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_source : Node_to_node_multimap.t
    ; targeted_dynamic_pop_edges_by_source : Node_to_node_and_targeted_dynamic_pop_action_multimap.t
    ; untargeted_dynamic_pop_actions_by_source : Node_to_untargeted_dynamic_pop_action_multimap.t
    ; push_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_target : Node_to_node_multimap.t
    ; push_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    ; pop_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    }
  [@@deriving show]
  ;;

  let compare x y =
    let c1 = Node_to_node_and_stack_element_multimap.compare
        x.push_edges_by_source y.push_edges_by_source
    in
    if c1 <> 0
    then c1
    else
      let c2 = Node_to_node_and_stack_element_multimap.compare
          x.pop_edges_by_source y.pop_edges_by_source
      in
      if c2 <> 0
      then c2
      else
        let c3 = Node_to_node_multimap.compare
            x.nop_edges_by_source y.nop_edges_by_source
        in
        if c3 <> 0
        then c3
        else
          let c4 =
            Node_to_node_and_targeted_dynamic_pop_action_multimap.compare
              x.targeted_dynamic_pop_edges_by_source
              y.targeted_dynamic_pop_edges_by_source
          in
          if c4 <> 0
          then c4
          else
            let c5 =
              Node_to_untargeted_dynamic_pop_action_multimap.compare
                x.untargeted_dynamic_pop_actions_by_source
                y.untargeted_dynamic_pop_actions_by_source
            in
            if c5 <> 0
            then c5
            else 0
  ;;
  let equal x y = compare x y == 0;;

  let to_yojson x =
    `Assoc
      [ ( "push_edges_by_source"
        , Node_to_node_and_stack_element_multimap.to_yojson
            x.push_edges_by_source
        )
      ; ( "pop_edges_by_source"
        , Node_to_node_and_stack_element_multimap.to_yojson
            x.pop_edges_by_source
        )
      ; ( "nop_edges_by_source"
        , Node_to_node_multimap.to_yojson x.nop_edges_by_source
        )
      ; ( "targeted_dynamic_pop_edges_by_source"
        , Node_to_node_and_targeted_dynamic_pop_action_multimap.to_yojson
            x.targeted_dynamic_pop_edges_by_source
        )
      ; ( "untargeted_dynamic_pop_actions_by_source"
        , Node_to_untargeted_dynamic_pop_action_multimap.to_yojson
            x.untargeted_dynamic_pop_actions_by_source
        )
      ]
  ;;

  let to_yojson_delta x y =
    let multimap_delta enum of_enum mem x y =
      y
      |> enum
      |> Enum.filter
        (fun (k,v) -> not @@ mem k v x)
      |> of_enum
    in
    `Assoc
      (List.filter
         (fun (_,v) ->
            match v with
            | `List n -> not @@ List.is_empty n
            | _ -> true
         )
         [ ( "push_edges_by_source"
           , Node_to_node_and_stack_element_multimap.to_yojson @@
             multimap_delta
               Node_to_node_and_stack_element_multimap.enum
               Node_to_node_and_stack_element_multimap.of_enum
               Node_to_node_and_stack_element_multimap.mem
               x.push_edges_by_source y.push_edges_by_source
           )
         ; ( "pop_edges_by_source"
           , Node_to_node_and_stack_element_multimap.to_yojson @@
             multimap_delta
               Node_to_node_and_stack_element_multimap.enum
               Node_to_node_and_stack_element_multimap.of_enum
               Node_to_node_and_stack_element_multimap.mem
               x.pop_edges_by_source y.pop_edges_by_source
           )
         ; ( "nop_edges_by_source"
           , Node_to_node_multimap.to_yojson @@
             multimap_delta
               Node_to_node_multimap.enum
               Node_to_node_multimap.of_enum
               Node_to_node_multimap.mem
               x.nop_edges_by_source y.nop_edges_by_source
           )
         ; ( "targeted_dynamic_pop_edges_by_source"
           , Node_to_node_and_targeted_dynamic_pop_action_multimap.to_yojson @@
             multimap_delta
               Node_to_node_and_targeted_dynamic_pop_action_multimap.enum
               Node_to_node_and_targeted_dynamic_pop_action_multimap.of_enum
               Node_to_node_and_targeted_dynamic_pop_action_multimap.mem
               x.targeted_dynamic_pop_edges_by_source
               y.targeted_dynamic_pop_edges_by_source
           )
         ; ( "untargeted_dynamic_pop_actions_by_source"
           , Node_to_untargeted_dynamic_pop_action_multimap.to_yojson @@
             multimap_delta
               Node_to_untargeted_dynamic_pop_action_multimap.enum
               Node_to_untargeted_dynamic_pop_action_multimap.of_enum
               Node_to_untargeted_dynamic_pop_action_multimap.mem
               x.untargeted_dynamic_pop_actions_by_source
               y.untargeted_dynamic_pop_actions_by_source
           )
         ]
      )
  ;;

  let empty =
    { push_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_source = Node_to_node_multimap.empty
    ; targeted_dynamic_pop_edges_by_source =
        Node_to_node_and_targeted_dynamic_pop_action_multimap.empty
    ; untargeted_dynamic_pop_actions_by_source =
        Node_to_untargeted_dynamic_pop_action_multimap.empty
    ; push_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_target = Node_to_node_multimap.empty
    ; push_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    };;

  let has_edge edge analysis =
    match edge.edge_action with
    | Nop ->
      analysis.nop_edges_by_source
      |> Node_to_node_multimap.mem edge.source edge.target
    | Push element ->
      analysis.push_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
    | Pop element ->
      analysis.pop_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
    | Pop_dynamic_targeted action ->
      analysis.targeted_dynamic_pop_edges_by_source
      |> Node_to_node_and_targeted_dynamic_pop_action_multimap.mem
        edge.source (edge.target, action)
  ;;

  let add_edge edge structure =
    match edge.edge_action with
    | Nop ->
      { structure with
        nop_edges_by_source =
          structure.nop_edges_by_source
          |> Node_to_node_multimap.add edge.source edge.target
      ; nop_edges_by_target =
          structure.nop_edges_by_target
          |> Node_to_node_multimap.add edge.target edge.source
      }
    | Push element ->
      { structure with
        push_edges_by_source =
          structure.push_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (edge.target, element)
      ; push_edges_by_target =
          structure.push_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (edge.source, element)
      ; push_edges_by_source_and_element =
          structure.push_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.source, element) edge.target
      }
    | Pop element ->
      { structure with
        pop_edges_by_source =
          structure.pop_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (edge.target, element)
      ; pop_edges_by_target =
          structure.pop_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (edge.source, element)
      ; pop_edges_by_source_and_element =
          structure.pop_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.source, element) edge.target
      }
    | Pop_dynamic_targeted action ->
      { structure with
        targeted_dynamic_pop_edges_by_source =
          structure.targeted_dynamic_pop_edges_by_source
          |> Node_to_node_and_targeted_dynamic_pop_action_multimap.add
            edge.source (edge.target, action)
      }
  ;;

  let add_untargeted_dynamic_pop_action source action structure =
    { structure with
      untargeted_dynamic_pop_actions_by_source =
        structure.untargeted_dynamic_pop_actions_by_source
        |> Node_to_untargeted_dynamic_pop_action_multimap.add
          source action
    }
  ;;

  let has_untargeted_dynamic_pop_action source action structure =
    structure.untargeted_dynamic_pop_actions_by_source
    |> Node_to_untargeted_dynamic_pop_action_multimap.mem source action
  ;;

  let find_push_edges_by_source source structure =
    Node_to_node_and_stack_element_multimap.find
      source structure.push_edges_by_source
  ;;

  let find_pop_edges_by_source source structure =
    Node_to_node_and_stack_element_multimap.find
      source structure.pop_edges_by_source
  ;;

  let find_nop_edges_by_source source structure =
    Node_to_node_multimap.find source structure.nop_edges_by_source
  ;;

  let find_targeted_dynamic_pop_edges_by_source source structure =
    Node_to_node_and_targeted_dynamic_pop_action_multimap.find source
      structure.targeted_dynamic_pop_edges_by_source
  ;;

  let find_untargeted_dynamic_pop_actions_by_source source structure =
    Node_to_untargeted_dynamic_pop_action_multimap.find source
      structure.untargeted_dynamic_pop_actions_by_source
  ;;

  let find_push_edges_by_target target structure =
    Node_to_node_and_stack_element_multimap.find
      target structure.push_edges_by_target
  ;;

  let find_pop_edges_by_target target structure =
    Node_to_node_and_stack_element_multimap.find
      target structure.pop_edges_by_target
  ;;

  let find_nop_edges_by_target target structure =
    Node_to_node_multimap.find target structure.nop_edges_by_target
  ;;

  let enumerate_nodes structure =
    Node_set.enum @@ Node_set.of_enum @@ Enum.concat @@ List.enum
      [ Node_to_node_and_stack_element_multimap.keys
          structure.push_edges_by_source
      ; Node_to_node_and_stack_element_multimap.keys
          structure.pop_edges_by_source
      ; Node_to_node_multimap.keys
          structure.nop_edges_by_source
      ; Node_to_node_and_targeted_dynamic_pop_action_multimap.keys
          structure.targeted_dynamic_pop_edges_by_source
      ; Node_to_untargeted_dynamic_pop_action_multimap.keys
          structure.untargeted_dynamic_pop_actions_by_source
      ; Node_to_node_and_stack_element_multimap.keys
          structure.push_edges_by_target
      ; Node_to_node_and_stack_element_multimap.keys
          structure.pop_edges_by_target
      ; Node_to_node_multimap.keys
          structure.nop_edges_by_target
      ; Enum.map (fst % snd)
          (Node_to_node_and_targeted_dynamic_pop_action_multimap.enum
             structure.targeted_dynamic_pop_edges_by_source)
      ]
  ;;

  let enumerate_edges structure =
    Enum.concat @@ List.enum
      [ Node_to_node_and_stack_element_multimap.enum
          structure.push_edges_by_source
        |> Enum.map
          (fun (source,(target,element)) ->
             { source = source
             ; target = target
             ; edge_action = Push element
             })
      ; Node_to_node_and_stack_element_multimap.enum
          structure.pop_edges_by_source
        |> Enum.map
          (fun (source,(target,element)) ->
             { source = source
             ; target = target
             ; edge_action = Pop element
             })
      ; Node_to_node_multimap.enum
          structure.nop_edges_by_source
        |> Enum.map
          (fun (source,target) ->
             { source = source
             ; target = target
             ; edge_action = Nop
             })
      ; Node_to_node_and_targeted_dynamic_pop_action_multimap.enum
          structure.targeted_dynamic_pop_edges_by_source
        |> Enum.map
          (fun (source,(target,dynamic_pop)) ->
             { source = source
             ; target = target
             ; edge_action = Pop_dynamic_targeted dynamic_pop
             })
      ]
  ;;
end;;
