open Utils
open List

module type G = sig
  type t
  type v
  val vertices : t -> v list
  val successors : t -> v -> v list
  val equal : v -> v -> bool
  val compare : v -> v -> int
end

module Make(G : G) = struct

  open G

  let rec assoc_opt_by p k = function
    | [] -> None
    | (k',v)::kvs ->
        if p k k' then Some v
        else assoc_opt_by p k kvs

  let sccs : t -> v list list = fun t ->
    let rec f cntr vns s p sccs (v : v) =
      let ws = successors t v in
      let vns = (v,cntr) :: vns in
      let s = v :: s in
      let p = (v,cntr) :: p in
      let cntr = cntr + 1 in
      let cntr, vns, s, p, sccs =
        fold_left (fun (cntr, vns, s, p, sccs) w ->
          match assoc_opt_by equal w vns with
          | None -> f cntr vns s p sccs w
          | Some n ->
              let rec pop = function
                | ((_,n')::_ as p) when n' <= n -> p
                | _::vns -> pop vns
                | [] -> assert false
              in
              cntr, vns, s, pop p, sccs) (cntr, vns, s, p, sccs) ws
      in
      match p with
      | [] -> assert false
      | (v',_) :: p when equal v v' ->
          let rec pop scc = function
            | v'::s ->
                if equal v v' then (v'::scc), s
                else pop (v'::scc) s
            | _ -> assert false
          in
          let scc, s = pop [] s in
          cntr, vns, s, p, scc::sccs
      | _ -> cntr, vns, s, p, sccs
    in
    let vs = vertices t in
    let _, _, _, sccs = 
      fold_left (fun (vns, s, p, sccs) v ->
        match assoc_opt_by equal v vns with
        | None ->
            let _, vns, s, p, sccs = f 0 vns s p sccs v in
            vns, s, p, sccs
        | Some _ -> vns, s, p, sccs) ([], [], [], []) vs
    in
    sccs

  let div_by_components : t -> v list list -> (v list * v list list) list = fun t cs ->
    let succs_v c = sort_uniq compare @@ concat_map (fun v -> successors t v) c in
    let mem v c = try ignore @@ find (equal v) c; true with Not_found -> false in
    let comp v = find (mem v) cs in
    (* using the following properties:
       * each components are non empty 
       * distinct each other 
     *)
    let succs_c c = sort_uniq (fun c1 c2 -> compare (hd c1) (hd c2)) @@ map comp @@ succs_v c in
    map (fun c -> c, succs_c c) cs

  let toposort : t -> v list option = fun t ->
    try
      (fun x -> Some x) @@ fst @@ fold_left (fun (res, tmp) v ->
        if mem v res then (res, tmp)
        else
          let rec visit res tmp v =
            if mem v tmp then raise Exit (* cycle *)
            else if mem v res then (res, tmp)
            else
              let tmp' = v :: tmp in
              let res, _ = fold_left (fun (res, tmp) v -> visit res tmp v) (res, tmp') (successors t v) in
              (v :: res, tmp)
          in
          visit res tmp v) ([], []) @@ vertices t
    with
    | Exit -> None
end
  
    
    
(* test code with ocamlgraph 

let make seed size =
  Random.init seed;
  let rec f st i =
    if i = size then st
    else begin
      let rec g st i =
        if i = size then st
        else g (if Random.float 1.0 < 0.3 then i::st else st) (i+1)
      in
      f ((i, g [] 0)::st) (i+1)
    end
  in
  f [] 0
  
module G = struct
  type t = (int * int list) list
  module V = struct
    type t = int * int list
    let compare (i1, _) (i2, _) = compare i1 i2
    let hash (i, _) = Hashtbl.hash i
    let equal (i1, _) (i2, _) = i1 = i2 
  end
  let iter_vertex f t = List.iter f t
  let iter_succ f (t : t) (_,vs) = List.iter (fun v -> f (List.find (fun (k,_) -> k = v) t)) vs
end

module GC = Graph.Components.Make(G)
  
let test seed =
  let g = make seed 100 in
  let a1 = sort compare @@ List.map (sort compare) @@ sccs g in
  let a2 = sort compare @@ List.map (fun x -> sort compare @@ List.map fst x) @@ GC.scc_list g in
  assert (a1 = a2)

let () =
  for i = 0 to 10000 do test i done
*)

    
