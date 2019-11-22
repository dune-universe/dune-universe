open! Import
include Topological_sort_intf

module Edge = struct
  type 'a t =
    { from : 'a
    ; to_ : 'a
    }
  [@@deriving compare, sexp_of]

  let equal equal_a t1 t2 = equal_a t1.from t2.from && equal_a t1.to_ t2.to_
  let map t ~f = { from = f t.from; to_ = f t.to_ }
end

let check_result
      (type node)
      (module Node : Node with type t = node)
      nodes
      (edges : Node.t Edge.t list)
      result
  =
  try
    match result with
    | Ok sorted ->
      let index_by_node =
        List.mapi sorted ~f:(fun i node -> node, i) |> Hashtbl.of_alist_exn (module Node)
      in
      let index_of node =
        match Hashtbl.find index_by_node node with
        | Some i -> i
        | None -> raise_s [%message "sort output missing node" ~_:(node : Node.t)]
      in
      List.iter nodes ~f:(fun node -> ignore (index_of node : int));
      List.iter edges ~f:(fun edge ->
        if index_of edge.from >= index_of edge.to_
        then
          raise_s
            [%message "sort output did not respect edge" ~_:(edge : Node.t Edge.t)])
    | Error cycle ->
      (match cycle with
       | [] -> failwith "cycle unexpectedly empty"
       | first :: rest ->
         let assert_edge edge =
           if not (List.mem edges edge ~equal:(Edge.equal Node.equal))
           then raise_s [%message "missing edge" ~_:(edge : Node.t Edge.t)]
         in
         let rec check_cycle node rest =
           match rest with
           | [] -> assert_edge { from = node; to_ = first }
           | node2 :: rest ->
             assert_edge { from = node; to_ = node2 };
             check_cycle node2 rest
         in
         check_cycle first rest)
  with
  | exn ->
    raise_s
      [%sexp
        "Topological_sort detected bug"
      , { exn : exn
        ; result : (Node.t list, Node.t list) Result.t
        ; nodes : Node.t list
        ; edges : Node.t Edge.t list
        }]
;;

let sort
      (type node)
      ?(verbose = false)
      (module Node : Node with type t = node)
      (nodes : node list)
      (edges : node Edge.t list)
  =
  let module Node_info = struct
    exception Cycle of node list

    type state =
      | Unvisited
      | Visiting
      | Visited
    [@@deriving sexp_of]

    type t =
      { node : Node.t
      ; mutable state : state
      ; mutable num_incoming : int
      ; mutable outgoing : (t[@sexp.opaque]) list
      }
    [@@deriving sexp_of]

    let create node = { node; state = Unvisited; num_incoming = 0; outgoing = [] }

    let add_edge { Edge.from; to_ } =
      to_.num_incoming <- to_.num_incoming + 1;
      from.outgoing <- to_ :: from.outgoing
    ;;

    let is_isolated t = t.num_incoming = 0 && List.is_empty t.outgoing

    (* [visit t ~visiting ~visited] visits all nodes reachable from [t], returning all
       newly visited nodes added to the front of [visited] in topological order.
       [visiting] is the list of nodes with [state = Visiting]. *)
    let rec visit t ~visiting ~visited =
      if verbose
      then
        eprint_s
          [%sexp
            "visit"
          , [%here]
          , { node = (t.node : Node.t); visiting : Node.t list; visited : Node.t list }];
      match t.state with
      | Visited -> visited
      | Visiting ->
        let cycle =
          match List.findi visiting ~f:(fun _ node -> phys_equal node t.node) with
          | None -> assert false
          | Some (i, _) -> List.rev (List.take visiting (i + 1))
        in
        raise (Cycle cycle)
      | Unvisited ->
        t.state <- Visiting;
        let visiting = t.node :: visiting in
        let visited =
          List.fold t.outgoing ~init:visited ~f:(fun visited t ->
            visit t ~visiting ~visited)
        in
        t.state <- Visited;
        t.node :: visited
    ;;
  end
  in
  let info_by_node = Hashtbl.create (module Node) in
  let node_info node =
    Hashtbl.find_or_add info_by_node node ~default:(fun () -> Node_info.create node)
  in
  List.iter nodes ~f:(fun node -> ignore (node_info node : Node_info.t));
  List.iter edges ~f:(fun edge -> Node_info.add_edge (Edge.map edge ~f:node_info));
  (* We sort the nodes with isolated nodes before other nodes, and then in decreasing
     order of [Node.compare].  This visits isolated nodes first, putting them at the end
     of the topsort output.  Sorting also makes the output deterministic. *)
  let node_visit_order =
    Hashtbl.data info_by_node
    |> List.sort ~compare:(fun (n1 : Node_info.t) n2 ->
      match Node_info.is_isolated n1, Node_info.is_isolated n2 with
      | true, false -> -1
      | false, true -> 1
      | false, false | true, true -> Node.compare n2.node n1.node)
  in
  let result =
    match
      List.fold node_visit_order ~init:[] ~f:(fun visited node_info ->
        Node_info.visit node_info ~visiting:[] ~visited)
    with
    | visited -> Ok visited
    | exception Node_info.Cycle cycle -> Error cycle
  in
  check_result (module Node) nodes edges result;
  result
;;

let sort (type node) ?verbose (module Node : Node with type t = node) nodes edges =
  match sort ?verbose (module Node) nodes edges with
  | Ok _ as x -> x
  | Error cycle ->
    error_s [%message "Topological_sort.sort encountered cycle" ~_:(cycle : Node.t list)]
;;
