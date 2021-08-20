(* invariants:
   - A. a node downstream of an invalid node is invalid
   - B. a node upstream of a valid node is valid

   assumptions:
   - 1. in an expression [bind m f], the set of nodes X created during any evaluation of
        [f] is upstream of the result of the evaluation of [f] and any element of
        X is reachable through X by following upstream links
        -> this rules out a lot of weird graphs that could be constructed using mutability,
           and allows a very simple GC algorithm
*)

(* TODO:

   we use type `ex list` for [used_by], this is not very structured and not
   very efficient (cf the calls to `List.filter` to remove an element)
   - we could use a set or a hashtbl?
   - we could unroll `List.filter` on a dedicated list type (typically a node
     will not be used in many contexts so [used_by] should be small)
*)

module Counter = struct
  let gen =
    let x = ref 0 in
    fun () ->
      let v = !x in
      incr x ;
      v
end

type 'a t =
  { desc : 'a desc;
    uid : int;
    tag : string;
    mutable value : 'a option;
    mutable used_by : ex list;
    mutable watchers : ('a -> unit) list
  }

and 'a desc =
  | Return : 'a -> 'a desc
  | Var : { mutable v : 'a } -> 'a desc
  | Gen : { gen : unit -> 'a } -> 'a desc
  | Map : { x : 'a t; f : 'a -> 'b } -> 'b desc
  | Map2 : { x : 'a t; y : 'b t; f : 'a -> 'b -> 'c } -> 'c desc
  | Map3 : { x : 'a t; y : 'b t; z : 'c t; f : 'a -> 'b -> 'c -> 'd } -> 'd desc
  | Bind :
      { m : 'a t;
        f : 'a -> 'b t;
        mutable currently_allocated : (int * int * ex) option
      }
      -> 'b desc
  | If :
      { cond : bool t; iftrue : 'a t; iffalse : 'a t; mutable dyn : ex option }
      -> 'a desc

and ex = Ex : 'a t -> ex

let ex node = Ex node

let tag : type a. int -> a desc -> string =
  let open Format in
  fun uid desc ->
    match desc with
    | Return _ -> asprintf "return (%d)" uid
    | Var _ -> asprintf "var (%d)" uid
    | Map _ -> asprintf "map (%d)" uid
    | Map2 _ -> asprintf "map2 (%d)" uid
    | Map3 _ -> asprintf "map3 (%d)" uid
    | Bind _ -> asprintf "bind (%d)" uid
    | If _ -> asprintf "if (%d)" uid
    | Gen _ -> asprintf "gen (%d)" uid

let make desc =
  let uid = Counter.gen () in
  let tag = tag uid desc in
  { desc; uid; tag; value = None; used_by = []; watchers = [] }

let var x = x

let gen x = x

let rec invalidate : type a. a t -> unit =
 fun node ->
  match node.value with
  | None ->
      (* node is invalid, by invariant A all nodes downstream are already invalid *)
      ()
  | Some _ ->
      node.value <- None ;
      List.iter (fun (Ex n) -> invalidate n) node.used_by

module Var = struct
  type nonrec 'a t = 'a t

  let create v = make (Var { v })

  let peek var = match var.desc with Var var -> var.v | _ -> assert false

  let set var x =
    invalidate var ;
    match var.desc with Var var -> var.v <- x | _ -> assert false
end

module Gen = struct
  type nonrec 'a t = 'a t

  let create gen = make (Gen { gen })

  let touch gen_node = invalidate gen_node
end

let add_used_by usee user = usee.used_by <- user :: usee.used_by

let return x = make (Return x)

(* TODO: smart constructors (eg when input is Return ...)*)

let map x f =
  let node = make (Map { x; f }) in
  add_used_by x (ex node) ;
  node

let map2 x y f =
  let node = make (Map2 { x; y; f }) in
  let ex = ex node in
  add_used_by x ex ;
  add_used_by y ex ;
  node

let map3 x y z f =
  let node = make (Map3 { x; y; z; f }) in
  let ex = ex node in
  add_used_by x ex ;
  add_used_by y ex ;
  add_used_by z ex ;
  node

let bind m f =
  let node = make (Bind { m; f; currently_allocated = None }) in
  add_used_by m (ex node) ;
  node

let if_ cond iftrue iffalse =
  let node = make (If { cond; iftrue; iffalse; dyn = None }) in
  add_used_by cond (ex node) ;
  node

let on_update node f = node.watchers <- f :: node.watchers

let disconnect (low, high, root) =
  let rec loop low hi (Ex node) =
    let uid = node.uid in
    if low < uid && uid < hi then (
      node.used_by <- [] ;
      match node.desc with
      | Map { x; _ } -> loop low hi (ex x)
      | Map2 { x; y; _ } ->
          loop low hi (ex x) ;
          loop low hi (ex y)
      | Map3 { x; y; z; _ } ->
          loop low hi (ex x) ;
          loop low hi (ex y) ;
          loop low hi (ex z)
      | Bind { m; currently_allocated; _ } -> (
          match currently_allocated with
          | None -> loop low hi (ex m)
          | Some (low', hi', dyn) ->
              (* Dynamically created bind is set for deletion, recurse. *)
              loop low hi (ex m) ;
              loop low' hi' dyn)
      | If { cond; dyn; _ } ->
          loop low hi (ex cond) ;
          Option.iter (fun dyn -> loop low hi dyn) dyn
      | Return _ | Var _ | Gen _ -> ())
    else
      let used_by =
        List.filter
          (fun (Ex node) ->
            let uid = node.uid in
            not (low < uid && uid < hi))
          node.used_by
      in
      node.used_by <- used_by
  in
  loop low high root

let rec refresh : type a. a t -> a =
 fun node ->
  match node.value with
  | Some v -> v
  | None ->
      let v =
        match node.desc with
        | Return v -> v
        | Var { v } ->
            node.value <- Some v ;
            v
        | Gen { gen } ->
            let v = gen () in
            node.value <- Some v ;
            v
        | Map { x; f } ->
            let x = refresh x in
            let v = f x in
            node.value <- Some v ;
            v
        | Map2 { x; y; f } ->
            let x = refresh x in
            let y = refresh y in
            let v = f x y in
            node.value <- Some v ;
            v
        | Map3 { x; y; z; f } ->
            let x = refresh x in
            let y = refresh y in
            let z = refresh z in
            let v = f x y z in
            node.value <- Some v ;
            v
        | Bind ({ m; f; currently_allocated } as bind_state) ->
            Option.iter disconnect currently_allocated ;
            let m = refresh m in
            let low = Counter.gen () in
            let dyn = f m in
            let high = Counter.gen () in
            add_used_by dyn (ex node) ;
            bind_state.currently_allocated <- Some (low, high, Ex dyn) ;
            let v = refresh dyn in
            node.value <- Some v ;
            v
        | If ({ cond; iftrue; iffalse; dyn } as cond_state) -> (
            (match dyn with
            | None -> ()
            | Some (Ex active_branch) ->
                (* remove [node] from [active_branch.used_by]*)
                let used_by =
                  List.filter
                    (fun (Ex node') -> node'.uid <> node.uid)
                    active_branch.used_by
                in
                active_branch.used_by <- used_by) ;
            let cond = refresh cond in
            match cond with
            | true ->
                cond_state.dyn <- Some (ex iftrue) ;
                let v = refresh iftrue in
                node.value <- Some v ;
                add_used_by iftrue (ex node) ;
                v
            | false ->
                cond_state.dyn <- Some (ex iffalse) ;
                let v = refresh iffalse in
                node.value <- Some v ;
                add_used_by iffalse (ex node) ;
                v)
      in
      List.iter (fun watcher -> watcher v) node.watchers ;
      v

let get node = refresh node

module Infix = struct
  let ( let* ) m f = bind m f

  let ( >>= ) m f = bind m f

  let ( >|= ) = map
end

module Internal = struct
  type mode = Backward_only | Full

  module Node_ex = struct
    type t = ex

    let compare (Ex n1) (Ex n2) = Int.compare n1.uid n2.uid

    let equal (Ex n1) (Ex n2) = Int.equal n1.uid n2.uid

    let hash (Ex n) = n.uid
  end

  module Node_set = Set.Make (Node_ex)
  module Node_table = Hashtbl.Make (Node_ex)

  type edge = Dependency of ex | Dynamic of ex

  type graph = edge list Node_table.t

  let uid (Ex n) = n.uid

  let upstream (Ex n) =
    match n.desc with
    | Return _ -> []
    | Var _ -> []
    | Map { x; _ } -> [ex x]
    | Map2 { x; y; _ } -> [ex x; ex y]
    | Map3 { x; y; z; _ } -> [ex x; ex y; ex z]
    | Bind { m; _ } -> [ex m]
    | If { cond; dyn; _ } ->
        ex cond :: (match dyn with None -> [] | Some dyn -> [dyn])
    | Gen _ -> []

  let used_by (Ex n) = n.used_by

  let valid (Ex n) = Option.is_some n.value

  let rec list_mem node list =
    match list with
    | [] -> false
    | (Dependency hd | Dynamic hd) :: tl ->
        Node_ex.equal node hd || list_mem node tl

  let add_dependency (graph : graph) src dst =
    match Node_table.find_opt graph src with
    | None -> Node_table.add graph src [Dependency dst]
    | Some dsts ->
        let dsts = if list_mem dst dsts then dsts else Dependency dst :: dsts in
        Node_table.replace graph src dsts

  let add_dynamic (graph : graph) src dst =
    match Node_table.find_opt graph src with
    | None -> Node_table.add graph src [Dynamic dst]
    | Some dsts ->
        let dsts = if list_mem dst dsts then dsts else Dynamic dst :: dsts in
        Node_table.replace graph src dsts

  let compute_graph (mode : mode) node =
    let rec loop graph visited (node : ex) =
      if Node_set.mem node visited then ()
      else
        let visited = Node_set.add node visited in
        let (Ex n) = node in
        let upstream = upstream node in
        List.iter (fun back -> add_dependency graph back node) upstream ;
        (match n.desc with
        | Bind { currently_allocated = Some (_, _, dyn); _ }
        | If { dyn = Some dyn; _ } ->
            loop graph visited dyn ;
            add_dynamic graph dyn node
        | _ -> ()) ;
        List.iter (fun back -> loop graph visited back) upstream ;
        match mode with
        | Backward_only -> ()
        | Full -> List.iter (fun fwd -> loop graph visited fwd) n.used_by
    in
    let visited = Node_set.empty in
    let graph = Node_table.create 511 in
    Node_table.add graph node [] ;
    loop graph visited node ;
    graph

  let to_adjacency_list (graph : edge list Node_table.t) =
    let nodes = Array.of_seq (Node_table.to_seq_keys graph) in
    Array.map (fun node -> (node, Node_table.find graph node)) nodes

  let to_dot ?(name = "export") ~mode node oc =
    let graph = compute_graph mode node in
    let adj = to_adjacency_list graph in
    let fmtr = Format.formatter_of_out_channel oc in
    Format.fprintf fmtr "digraph %s {@." name ;
    Array.iter
      (fun (Ex node, _) ->
        Format.fprintf
          fmtr
          "node_%d [%slabel=\"%s_%d\"];@."
          node.uid
          (if valid (ex node) then "" else "shape = box ")
          node.tag
          node.uid)
      adj ;
    let dynamic_attr =
      {|[ penwidth = 2 fontsize = 28 fontcolor = "black" label = "dyn" ]|}
    in
    Array.iter
      (fun (Ex node, edges) ->
        List.iter
          (function
            | Dependency (Ex node') ->
                Format.fprintf fmtr "node_%d -> node_%d;@." node.uid node'.uid
            | Dynamic (Ex node') ->
                Format.fprintf
                  fmtr
                  "node_%d -> node_%d %s;@."
                  node.uid
                  node'.uid
                  dynamic_attr)
          edges)
      adj ;
    Format.fprintf fmtr "}@." ;
    flush oc
end
