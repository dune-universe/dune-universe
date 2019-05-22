(**
   This module defines the actual PDS reachability analysis.
*)

open Batteries;;
open Jhupllib;;

open Eager_nondeterminism;;
open Pp_utils;;
open Yojson_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Pds_reachability_analysis";;

module type Analysis =
sig
  include Pds_reachability_types.Types;;

  (** The type of edge-generating functions used in this analysis. *)
  type edge_function = State.t -> (Stack_action.t list * Terminus.t) Enum.t

  (** The type of functions to generate untargeted dynamic pop actions in this
      analysis. *)
  type untargeted_dynamic_pop_action_function =
    State.t -> Untargeted_dynamic_pop_action.t Enum.t

  (** The type of a reachability analysis in this module. *)
  type analysis

  (** The type of reachability analysis logging functions.  These functions
      are called whenever the associated analysis is stepped.  The two arguments
      are the analysis before stepping and the analysis after. *)
  type analysis_logging_function = analysis -> analysis -> unit

  (** The empty analysis.  This analysis has no states, edges, or edge
      functions. *)
  val empty :
    ?logging_function:(analysis_logging_function option) -> unit -> analysis

  (** Adds a single edge to a reachability analysis. *)
  val add_edge
    : State.t -> Stack_action.t list -> State.t -> analysis -> analysis

  (** Adds a function to generate edges for a reachability analysis.  Given a
      source node, the function generates edges from that source node.  The
      function must be pure; for a given source node, it must generate all edges
      that it can generate on the first call. *)
  val add_edge_function : edge_function -> analysis -> analysis

  (** Adds an untargeted pop action to a reachability analysis.  Untargeted pop
      action are similar to targeted pop actions except that they are not
      created as an edge with a target node; instead, the target is decided in
      some way by the pushed element that the untargeted dynamic pop is
      consuming. *)
  val add_untargeted_dynamic_pop_action
    : State.t -> Untargeted_dynamic_pop_action.t -> analysis -> analysis

  (** Adds a function to generate untargeted dynamic pop ations for a
      reachability analysis.  Given a source node, the function generates
      untargeted actions from that source node.  The function must be pure; for
      a given source, it must generate all actions that it can generate on the
      first call. *)
  val add_untargeted_dynamic_pop_action_function
    : untargeted_dynamic_pop_action_function -> analysis -> analysis

  (** Adds a state and initial stack element to the analysis.  This permits the
      state to be used as the source state of a call to [get_reachable_states].
  *)
  val add_start_state
    : State.t -> Stack_action.t list -> analysis -> analysis

  (** Determines whether the reachability analysis is closed. *)
  val is_closed : analysis -> bool

  (** Takes a step toward closing a given reachability analysis.  If the
      analysis is already closed, it is returned unchanged. *)
  val closure_step : analysis -> analysis

  (** Fully closes the provided analysis. *)
  val fully_close : analysis -> analysis

  (** Determines the states which are reachable from a given state and initial
      stack element.  This state must have been added to the analysis
      previously.  If the analysis is not fully closed, then the enumeration of
      reachable states may be incomplete.  *)
  val get_reachable_states
    : State.t -> Stack_action.t list -> analysis -> State.t Enum.t

  (** Pretty-printing function for the analysis. *)
  val pp_analysis : analysis pretty_printer
  val show_analysis : analysis -> string

  (** An exception raised when a reachable state query occurs before the state
      is added as a start state. *)
  exception Reachability_request_for_non_start_state of State.t;;

  (** Determines the size of the provided analysis in terms of both node and
      edge count (respectively). *)
  val get_size : analysis -> int * int

  (** Determines the amount of work done on a particular analysis. *)
  val get_work_count : analysis -> int

  (** Extracts a subset of information about an analysis state as JSON data.
      Some parts of the analysis state (such as edge functions) will be
      elided as they cannot be represented. *)
  val dump_yojson : analysis -> Yojson.Safe.json

  (** Extracts a subset of information about an analysis state as JSON data.
      This extraction generates a /difference/ between two reachability analyses,
      giving values appearing in the latter but not the former.  This function
      assumes the latter is a strict superset of the former; any values appearing
      in the former and not the latter are ignored.  The format of this dump is
      identical to that given by [dump_yojson]. *)
  val dump_yojson_delta : analysis -> analysis -> Yojson.Safe.json
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State)
    (Work_collection_template_impl :
       Pds_reachability_work_collection.Work_collection_template)
  : Analysis
    with module State = Basis.State
     and module Stack_element = Basis.Stack_element
     and module Targeted_dynamic_pop_action = Dph.Targeted_dynamic_pop_action
     and module Untargeted_dynamic_pop_action = Dph.Untargeted_dynamic_pop_action
     and module Stack_action = Dph.Stack_action
     and module Terminus = Dph.Terminus
=
struct
  (********** Create and wire in appropriate components. **********)

  module Types = Pds_reachability_types.Make(Basis)(Dph);;
  module Work = Pds_reachability_work.Make(Basis)(Types);;
  module Work_collection_impl = Work_collection_template_impl(Work);;
  module Structure = Pds_reachability_structure.Make(Basis)(Dph)(Types);;

  include Types;;
  open Types.Stack_action.T;;
  open Types.Terminus.T;;

  type edge_function = State.t -> (Stack_action.t list * Terminus.t) Enum.t;;
  type untargeted_dynamic_pop_action_function =
    State.t -> Untargeted_dynamic_pop_action.t Enum.t;;

  (********** Define utility data structures. **********)

  exception Reachability_request_for_non_start_state of State.t;;

  module State_set = struct
    module Impl = Set.Make(Basis.State);;
    include Impl;;
    include Set_pp(Impl)(Basis.State);;
    include Set_to_yojson(Impl)(Basis.State);;
  end;;

  module Node_set = struct
    module Impl = Set.Make(Node);;
    include Impl;;
    include Set_pp(Impl)(Node);;
    include Set_to_yojson(Impl)(Node);;
  end;;

  module Node_map = struct
    module Impl = Map.Make(Node);;
    include Impl;;
    include Map_pp(Impl)(Node);;
    include Map_to_yojson(Impl)(Node);;
  end;;

  (********** Define analysis structure. **********)

  type node_awareness =
    | Seen
    (* Indicates that this node exists somewhere in the work queue but not in
       the analysis structure. *)
    | Expanded
    (* Indicates that this node exists somewhere in the analysis structure and
       has already been expanded.  An expanded node responds to edge functions,
       for instance. *)
  [@@deriving eq, show, to_yojson]
  let _ = show_node_awareness;; (* To ignore an unused generated function. *)

  type analysis =
    { node_awareness_map : node_awareness Node_map.t
    (* A mapping from each node to whether the analysis is aware of it.  Any node
       not in this map has not been seen in any fashion.  Every node that has been
       seen will be mapped to an appropriate [node_awareness] value. *)
    ; known_states : State_set.t
    (* A collection of all states appearing somewhere within the reachability
       structure (whether they have been expanded or not). *)
    ; start_nodes : Node_set.t
    (* A collection of the nodes which are recognized starting points.  Nop
       closure is only valid when sourced from these nodes, so these are the
       only nodes from which reachability questions are appropriate. *)
    ; reachability : Structure.t
    (* The underlying structure maintaining the nodes and edges in the graph. *)
    ; edge_functions : edge_function list
          [@printer fun formatter functions ->
            Format.fprintf formatter "(length = %d)"
              (List.length functions)]
    (* The list of all edge functions for this analysis. *)
    ; untargeted_dynamic_pop_action_functions :
        untargeted_dynamic_pop_action_function list
          [@printer fun formatter functions ->
            Format.fprintf formatter "(length = %d)"
              (List.length functions)]
    (* The list of all untargeted dynamic pop action functions. *)
    ; work_collection : Work_collection_impl.work_collection
    (* The collection of work which has not yet been performed. *)
    ; work_count : int
    (* The number of work steps that have been taken in this analysis. *)
    ; logging_function : analysis_logging_function option
          [@printer fun formatter fn ->
            Format.fprintf formatter "logging function %s"
              (if Option.is_some fn then "present" else "absent")]
          (* The logging function to use when work is done on this analysis. *)
    }
  [@@deriving show]
and analysis_logging_function = analysis -> analysis -> unit
;;

let _ = pp_analysis_logging_function;;
let _ = show_analysis_logging_function;;

(********** Analysis utility functions. **********)

let add_work work analysis =
  lazy_logger `trace (fun _ ->
      Printf.sprintf "Adding work: %s" (Work.show work)
    );
  match work with
  | Work.Expand_node node ->
    if Node_map.mem node analysis.node_awareness_map
    then analysis
    else
      { analysis with
        work_collection =
          Work_collection_impl.offer work analysis.work_collection
      ; node_awareness_map =
          Node_map.add node Seen analysis.node_awareness_map
      }
  | Work.Introduce_edge edge ->
    (* TODO: We might want to filter duplicate introduce-edge steps from the
       work collection. *)
    if Structure.has_edge edge analysis.reachability
    then analysis
    else
      { analysis with
        work_collection =
          Work_collection_impl.offer work analysis.work_collection
      }
  | Work.Introduce_untargeted_dynamic_pop(from_node,action) ->
    (* TODO: We might want to filter duplicate introduce-udynpop steps from
       the work collection. *)
    if Structure.has_untargeted_dynamic_pop_action
        from_node action analysis.reachability
    then analysis
    else
      { analysis with
        work_collection =
          Work_collection_impl.offer work analysis.work_collection
      }
;;

let add_works works analysis = Enum.fold (flip add_work) analysis works;;

let create_work_for_path
    (from_node : node)
    (actions : Stack_action.t list)
    (destination : intermediate_destination)
  : Work.t =
  match actions,destination with
  | [], Static_destination to_node ->
    Work.Introduce_edge {source=from_node;target=to_node;edge_action=Nop}
  | [], Dynamic_destination udynpop ->
    Work.Introduce_untargeted_dynamic_pop(from_node, udynpop)
  | action::[], Static_destination to_node ->
    Work.Introduce_edge {source=from_node;target=to_node;edge_action=action}
  | action::actions, _ ->
    let to_node' = Intermediate_node(destination, actions) in
    Work.Introduce_edge {source=from_node;target=to_node';edge_action=action}
;;

let terminus_to_destination terminus =
  match terminus with
  | Static_terminus state -> Static_destination (State_node state)
  | Dynamic_terminus udynpop -> Dynamic_destination udynpop
;;

(********** Define analysis operations. **********)

let get_size analysis =
  let reachability = analysis.reachability in
  let node_count = Enum.count @@ Structure.enumerate_nodes reachability in
  let edge_count = Enum.count @@ Structure.enumerate_edges reachability in
  (node_count, edge_count)
;;

let empty ?logging_function:(log_fn=None) () =
  { node_awareness_map = Node_map.empty
  ; known_states = State_set.empty
  ; start_nodes = Node_set.empty
  ; reachability = Structure.empty
  ; edge_functions = []
  ; untargeted_dynamic_pop_action_functions = []
  ; work_collection = Work_collection_impl.empty
  ; work_count = 0
  ; logging_function = log_fn
  };;

let add_edge from_state stack_action_list to_state analysis =
  let from_node = State_node from_state in
  let to_node = State_node to_state in
  let path_work =
    create_work_for_path
      from_node stack_action_list (Static_destination to_node)
  in
  (* Add work for the source node and the edge. *)
  analysis
  |> add_work (Work.Expand_node from_node)
  |> add_work path_work
  (* When the path_work is executed, it will add the destination node if
     necessary. *)
;;

let add_edge_function (edge_function : edge_function) analysis =
  (* First, we have to catch up on this edge function by calling it with every
     state present in the analysis. *)
  let work : Work.t Enum.t =
    let open Nondeterminism_monad in Nondeterminism_monad.enum @@
    let%bind from_state = pick_enum @@ State_set.enum analysis.known_states in
    let%bind (actions,terminus) = pick_enum @@ edge_function from_state in
    (* We know that the from_node has already been introduced, so we just have
       to add the edge.*)
    return @@ create_work_for_path
      (State_node from_state) actions (terminus_to_destination terminus)
  in
  (* Now we add both the catch-up work (so the analysis is as if the edge
     function was present all along) and the edge function (so it'll stay
     in sync in the future). *)
  { (add_works work analysis) with
    edge_functions = edge_function :: analysis.edge_functions
  }
;;

let add_untargeted_dynamic_pop_action from_state pop_action analysis =
  let from_node = State_node from_state in
  analysis
  |> add_work (Work.Expand_node from_node)
  |> add_work (Work.Introduce_untargeted_dynamic_pop(from_node, pop_action))
;;

let add_untargeted_dynamic_pop_action_function
    (pop_action_fn : untargeted_dynamic_pop_action_function) analysis =
  (* First, we have to catch up on this function by calling it with every
     state we know about. *)
  let work : Work.t Enum.t =
    let open Nondeterminism_monad in Nondeterminism_monad.enum @@
    let%bind from_state = pick_enum @@ State_set.enum analysis.known_states in
    let%bind action = pick_enum @@ pop_action_fn from_state in
    let from_node = State_node from_state in
    return @@ Work.Introduce_untargeted_dynamic_pop(from_node, action)
  in
  (* Now we add both the catch-up work (so the analysis is as if the function
     was present all along) and the function (so it'll stay in sync in the
     future). *)
  { (add_works work analysis) with
    untargeted_dynamic_pop_action_functions =
      pop_action_fn :: analysis.untargeted_dynamic_pop_action_functions
  }
;;

let add_start_state state stack_actions analysis =
  let start_node =
    Intermediate_node(Static_destination(State_node(state)), stack_actions)
  in
  (* If we've not seen this state before, then we need to expand it.
     If we have, we need to consider nop closure for it to catch it up. *)
  let entry =
    Node_map.Exceptionless.find start_node analysis.node_awareness_map
  in
  if entry <> Some Expanded
  then
    (* This node isn't in our structure, so we just need to add it.  Nop
       expansion, edge generation, and so on will occur when the expansion
       work is actually done. *)
    { (analysis |> add_work (Work.Expand_node(start_node))) with
      start_nodes = analysis.start_nodes |> Node_set.add start_node
    }
  else
    (* We've seen this node before but it didn't used to be a start node.
       We need to perform nop closure on it now to maintain the invariant
       that start nodes are nop-closed.  We only need to perform one frontier
       of nop closure; the insertion of those new nop edges will begin the
       cascade of nop closure to re-establish the invariant. *)
    let edge_work =
      let open Nondeterminism_monad in Nondeterminism_monad.enum @@
      let%bind middle_node =
        pick_enum @@
        Structure.find_nop_edges_by_source start_node analysis.reachability
      in
      let%bind target_node =
        pick_enum @@
        Structure.find_nop_edges_by_source middle_node analysis.reachability
      in
      return @@  Work.Introduce_edge
        ({source=start_node;target=target_node;edge_action=Nop})
    in
    add_works edge_work analysis
;;

let is_closed analysis =
  Work_collection_impl.is_empty analysis.work_collection
;;

let closure_step analysis =
  let (new_work_collection, work_opt) =
    Work_collection_impl.take analysis.work_collection
  in
  let result_analysis =
    match work_opt with
    | None -> analysis
    | Some work ->
      lazy_logger `trace
        (fun () ->
           Printf.sprintf "PDS reachability closure step: %s"
             (Work.show work)
        );
      let analysis = { analysis with work_collection = new_work_collection } in
      (* A utility function to add a node to a set *only if* it needs to be
         expanded. *)
      (* TODO: consider - do we want to do this filtering in add_work or
         something?  The advantage here is that we don't build up a big set...
      *)
      let can_expand node =
        let entry =
          Node_map.Exceptionless.find node analysis.node_awareness_map
        in
        entry <> Some Expanded
      in
      match work with
      | Work.Expand_node node ->
        begin
          (* We're adding to the analysis a node that it does not contain. *)
          match node with
          | State_node(state) ->
            (* We just need to introduce this node to all of the edge
               functions that we have accumulated so far. *)
            let edge_work =
              analysis.edge_functions
              |> List.enum
              |> Enum.map (fun f -> f state)
              |> Enum.concat
              |> Enum.map (fun (actions,terminus) ->
                  (* We know that the from_node has already been
                     introduced. *)
                  create_work_for_path
                    (State_node state)
                    actions
                    (terminus_to_destination terminus)
                )
            in
            let popdynu_work =
              analysis.untargeted_dynamic_pop_action_functions
              |> List.enum
              |> Enum.map (fun f -> f state)
              |> Enum.concat
              |> Enum.map
                (fun action ->
                   Work.Introduce_untargeted_dynamic_pop(node, action)
                )
            in
            { (analysis |> add_works edge_work |> add_works popdynu_work) with
              known_states = analysis.known_states |> State_set.add state
            ; node_awareness_map =
                analysis.node_awareness_map
                |> Node_map.add node Expanded
            }
          | Intermediate_node(destination, actions) ->
            (* The only edge implied by an intermediate node is the one that
               moves along the action chain. *)
            let edge_work =
              create_work_for_path node actions destination
            in
            (* We now add a work item to introduce this edge.  That introduction
               will trigger the expansion of any target node if necessary.  We
               also have to mark the source node as expanded in the awareness
               map. *)
            { (analysis
               |> add_work edge_work
              ) with
              node_awareness_map =
                Node_map.add node Expanded analysis.node_awareness_map
            }
        end
      | Work.Introduce_edge edge ->
        let { source = from_node
            ; target = to_node
            ; edge_action = action
            } = edge
        in
        let analysis' =
          (* When an egde is introduced, all of the edges connecting to it
             should be closed with it. *)
          let edge_work =
            match action with
            | Nop ->
              (* Nop edges should close with any pushes that lead into them. *)
              let push_closure_work =
                let open Nondeterminism_monad in Nondeterminism_monad.enum @@
                let%bind (from_node', element) =
                  pick_enum @@ Structure.find_push_edges_by_target
                    from_node analysis.reachability
                in
                return @@ Work.Introduce_edge(
                  { source = from_node'
                  ; target = to_node
                  ; edge_action = Push element
                  }
                )
              in
              (* We should also close pairs of adjacent nop edges if the source
                 of the first edge is a start node.  This is done in two steps:
                 first checking to see if this new edge has any nops to its
                 left (source-wise) and then checking to see if this new edge
                 is leaving a start node and has any nops to its right
                 (target-wise). *)
              let left_nop_work =
                let open Nondeterminism_monad in
                Nondeterminism_monad.enum @@
                let%bind from_node' =
                  pick_enum @@ Structure.find_nop_edges_by_target
                    from_node analysis.reachability
                in
                [%guard (Node_set.mem from_node' analysis.start_nodes)];
                return @@ Work.Introduce_edge(
                  { source = from_node'
                  ; target = to_node
                  ; edge_action = Nop
                  }
                )
              in
              let right_nop_work =
                let open Nondeterminism_monad in
                Nondeterminism_monad.enum @@
                begin
                  [%guard (Node_set.mem from_node analysis.start_nodes)];
                  let%bind to_node' =
                    pick_enum @@ Structure.find_nop_edges_by_source
                      to_node analysis.reachability
                  in
                  return @@ Work.Introduce_edge(
                    { source = from_node
                    ; target = to_node'
                    ; edge_action = Nop
                    }
                  )
                end
              in
              (* Now put it all together. *)
              Enum.concat @@ List.enum
                [ push_closure_work
                ; left_nop_work
                ; right_nop_work
                ]
            | Push k ->
              (* Any nop, pop, or popdyn edges at the target of this push can
                 be closed. *)
              let nop_work =
                let open Nondeterminism_monad in Nondeterminism_monad.enum @@
                let%bind to_node' =
                  pick_enum @@ Structure.find_nop_edges_by_source
                    to_node analysis.reachability
                in
                return @@ Work.Introduce_edge(
                  { source = from_node
                  ; target = to_node'
                  ; edge_action = Push k
                  })
              in
              let pop_work =
                let open Nondeterminism_monad in Nondeterminism_monad.enum @@
                let%bind (to_node', element) =
                  pick_enum @@ Structure.find_pop_edges_by_source
                    to_node analysis.reachability
                in
                [%guard (Stack_element.equal k element)];
                return @@ Work.Introduce_edge(
                  { source = from_node
                  ; target = to_node'
                  ; edge_action = Nop
                  })
              in
              let popdynt_work =
                let open Nondeterminism_monad in Nondeterminism_monad.enum @@
                let%bind (to_node, popdynt_action) =
                  pick_enum @@
                  Structure.find_targeted_dynamic_pop_edges_by_source
                    to_node analysis.reachability
                in
                let%bind stack_actions =
                  pick_enum @@ Dph.perform_targeted_dynamic_pop k popdynt_action
                in
                return @@ create_work_for_path
                  from_node stack_actions (Static_destination to_node)
              in
              let popdynu_work =
                let open Nondeterminism_monad in Nondeterminism_monad.enum @@
                let%bind popdynu_action =
                  pick_enum @@
                  Structure.find_untargeted_dynamic_pop_actions_by_source
                    to_node analysis.reachability
                in
                let%bind (stack_actions,terminus) =
                  pick_enum @@ Dph.perform_untargeted_dynamic_pop
                    k popdynu_action
                in
                return @@ create_work_for_path
                  from_node stack_actions (terminus_to_destination terminus)
              in
              Enum.concat @@ List.enum
                [ nop_work ; pop_work ; popdynt_work ; popdynu_work ]
            | Pop k ->
              (* Pop edges can only close with the push edges that precede
                 them. *)
              let open Nondeterminism_monad in Nondeterminism_monad.enum @@
              let%bind (from_node', element) =
                pick_enum @@ Structure.find_push_edges_by_target
                  from_node analysis.reachability
              in
              [%guard (Stack_element.equal element k)];
              return @@ Work.Introduce_edge(
                { source = from_node'
                ; target = to_node
                ; edge_action = Nop
                })
            | Pop_dynamic_targeted action ->
              (* Dynamic pop edges can only close with push edges that precede
                 them. *)
              let open Nondeterminism_monad in Nondeterminism_monad.enum @@
              let%bind (from_node', element) =
                pick_enum @@ Structure.find_push_edges_by_target
                  from_node analysis.reachability
              in
              let%bind stack_actions =
                pick_enum @@ Dph.perform_targeted_dynamic_pop element action
              in
              return @@ create_work_for_path
                from_node' stack_actions (Static_destination to_node)
          in
          (* In the case of non-pop edges, the destination of the edge should
             also be expanded (if it has not already been expanded). *)
          let expand_work =
            match action with
            | Pop _
            | Pop_dynamic_targeted _ ->
              Enum.empty ()
            | _ when can_expand to_node ->
              Enum.singleton (Work.Expand_node to_node)
            | _ ->
              Enum.empty ()
          in
          add_works (Enum.append edge_work expand_work) analysis
        in
        { analysis' with
          reachability = analysis'.reachability |> Structure.add_edge edge
        }
      | Work.Introduce_untargeted_dynamic_pop(from_node,action) ->
        (* Untargeted dynamic pops can only close with the push edges that
           reach them.  Any targets of the resulting edges are candidates for
           expansion. *)
        let analysis' =
          let edge_work =
            let open Nondeterminism_monad in Nondeterminism_monad.enum @@
            let%bind (from_node', element) =
              pick_enum @@ Structure.find_push_edges_by_target
                from_node analysis.reachability
            in
            let%bind (stack_actions, terminus) =
              pick_enum @@ Dph.perform_untargeted_dynamic_pop element action
            in
            return @@ create_work_for_path
              from_node' stack_actions (terminus_to_destination terminus)
          in
          add_works edge_work analysis
        in
        { analysis' with
          reachability = analysis'.reachability
                         |> Structure.add_untargeted_dynamic_pop_action
                           from_node action
        }
  in
  let logged_analysis =
    { result_analysis with
      work_count = result_analysis.work_count + 1
    }
  in
  begin
    match logged_analysis.logging_function with
    | Some f -> f analysis logged_analysis
    | None -> ()
  end;
  logged_analysis
;;

let rec fully_close analysis =
  if is_closed analysis
  then analysis
  else fully_close @@ closure_step analysis
;;

let get_reachable_states state stack_actions analysis =
  lazy_logger `debug (fun () ->
      let (nodes,edges) = get_size analysis in
      Printf.sprintf
        "get_reachable_states: analysis has %d nodes and %d edges"
        nodes edges
    );
  let node =
    Intermediate_node(Static_destination(State_node(state)), stack_actions)
  in
  if Node_set.mem node analysis.start_nodes
  then
      (*
        If a state is reachable by empty stack from the given starting state,
        then there will be a nop edge to it.  It's that simple once closure
        is finished.
      *)
    analysis.reachability
    |> Structure.find_nop_edges_by_source node
    |> Enum.filter_map
      (fun node ->
         match node with
         | State_node state -> Some state
         | Intermediate_node _ -> None
      )
  else
    raise @@ Reachability_request_for_non_start_state state
;;

let get_work_count analysis = analysis.work_count;;

let dump_yojson analysis =
  `Assoc
    [ ( "node_awareness_map"
      , Node_map.to_yojson
          node_awareness_to_yojson
          analysis.node_awareness_map
      )
    ; ( "known_states"
      , State_set.to_yojson analysis.known_states
      )
    ; ( "start_nodes"
      , Node_set.to_yojson analysis.start_nodes
      )
    ; ( "reachability"
      , Structure.to_yojson analysis.reachability
      )
    ; ( "edge_function_count"
      , `Int (List.length analysis.edge_functions)
      )
    ; ( "untargeted_dynamic_pop_action_function_count"
      , `Int (List.length analysis.untargeted_dynamic_pop_action_functions)
      )
    ; ( "work_collection"
      , Work_collection_impl.to_yojson analysis.work_collection
      )
    ; ( "work_count"
      , `Int analysis.work_count
      )
    ; ( "logging_function_present"
      , `Bool (Option.is_some analysis.logging_function)
      )
    ]
;;

let dump_yojson_delta a1 a2 =
  `Assoc
    (List.filter_map identity
       [
         begin
           let enum_new_mappings _ =
             a2.node_awareness_map
             |> Node_map.enum
             |> Enum.filter
               (fun (k,v) ->
                  match Node_map.Exceptionless.find
                          k a1.node_awareness_map with
                  | Some(v') -> not @@ equal_node_awareness v v'
                  | None -> true
               )
           in
           if Enum.is_empty @@ enum_new_mappings ()
           then None
           else Some
               ( "node_awareness_map"
               , Yojson_utils.map_to_yojson
                   Node.to_yojson
                   node_awareness_to_yojson
                   enum_new_mappings a2
               )
         end
         ;
         begin
           let new_known_states =
             State_set.diff a2.known_states a1.known_states
           in
           if State_set.is_empty new_known_states
           then None
           else Some
               ( "known_states"
               , State_set.to_yojson new_known_states
               )
         end
         ;
         begin
           let new_start_nodes =
             Node_set.diff a2.start_nodes a1.start_nodes
           in
           if Node_set.is_empty new_start_nodes
           then None
           else Some
               ( "start_nodes"
               , Node_set.to_yojson new_start_nodes
               )
         end
         ;
         begin
           let reachability_json =
             Structure.to_yojson_delta a1.reachability a2.reachability
           in
           match reachability_json with
           | `Assoc [] -> None
           | _ -> Some ( "reachability", reachability_json )
         end
         ;
         begin
           let a1_edge_function_count = List.length a1.edge_functions in
           let a2_edge_function_count = List.length a2.edge_functions in
           if a1_edge_function_count = a2_edge_function_count
           then None
           else Some ("edge_function_count", `Int a2_edge_function_count)
         end
         ;
         begin
           let a1_udpa_count =
             List.length a1.untargeted_dynamic_pop_action_functions
           in
           let a2_udpa_count =
             List.length a2.untargeted_dynamic_pop_action_functions
           in
           if a1_udpa_count = a2_udpa_count
           then None
           else Some ("untargeted_dynamic_pop_action_function_count",
                      `Int a2_udpa_count)
         end
         ;
         begin
           if Enum.equal Work.equal
               (Work_collection_impl.enum a1.work_collection)
               (Work_collection_impl.enum a2.work_collection)
           then None
           else
             Some
               ( "work_collection"
               , Work_collection_impl.to_yojson a2.work_collection
               )
         end
         ; Some ( "work_count"
                , `Int a2.work_count
                )
       ]
    )
;;
end;;
