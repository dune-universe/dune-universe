(*
   Suppose we have 2 root hashes, rh1, rh2, where rh2 is a decendant of rh1.
   Let top1 and top2 are the top nodes of rh1 and rh2 respectively.

   A node n which is reachable from top2 is:
   
   * If the index of n is newer than the index of top1, n is unreachable
     from top1.
   
   * If the index of n is older than (or equal to) the index of top1, 
                                                        
       * If n is a small leaf which is cached, n may be unreachable from top1.
       * Otherwise, n is always reachable from top1.

   Using the above property, we can reduce the memory usage.

   Copied correctly in 2h30m for 22G data on SSD storage
   in my machine kurama.
*)

open Plebeia.Internal
open Utils.Open
open Result

let get_kind v = match v with
  | Node.Leaf _ -> "leaf"
  | Internal _  -> "internal"
  | Bud _       -> "bud"
  | Extender _  -> "extender"

(* Traverse the nodes from [c] and returns the new nodes not in [past_nodes].
   It also returns the "frontier", the first reachable nodes from [c] which 
   were in [past_nodes].  Frontier is for optimization to reduce the number 
   of past nodes to be checked at copying.

   This function assumes the following property: commits are atomic, i.e., 
   any node reachable from [c] older than the newest node in [past_nodes] 
   must be in [past_nodes].
   
   * [past_nodes]:  The past nodes.
*)
let find_new_nodes past_nodes c =
  let context = Cursor.context c in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in
  let past_max_i = try fst @@ Index.Map.max_binding past_nodes with Not_found -> Index.zero in
  Cursor.fold ~init:(Index.Map.empty, Index.Map.empty) c @@ fun (new_nodes, frontier) c ->
    let i = from_Some @@ Cursor.index c in
    let _c, v = Cursor.view c in
    match v with
    | Node.Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
        (* Small leaves.  
           Skip them, since they are handled by the cache system *)
        `Continue, (new_nodes, frontier)
    | _ when i > past_max_i ->
        (* New node, which cannot appear in past_nodes *)
        (* assert (not @@ Index.Map.mem i past_nodes); *)
        if Index.Map.mem i new_nodes then begin
          (* This is not a bug, but quite surprising to have shared node 
             in one commit *)
          Format.eprintf "*** Shared node found in one commit! %d@." 
            (Index.to_int i)
        end;
        `Continue, (Index.Map.add i v new_nodes, frontier)
    | _ ->
        (* This should be an old node.
           Stop the traversal here and add it to the frontier.
        *)
        match Index.Map.find_opt i past_nodes with
        | None -> 
            Format.eprintf "*** node #%d is not found in the past nodes@." 
              (Index.to_int i);
            assert false
        | Some copied_n ->
            `Up, (new_nodes, Index.Map.add i copied_n frontier)

(* Compare 2 trees, one is the committed version of the other, to find out
   which index each node is copied to.

   new_nodes : The original nodes which are copied.  
               They must be all connected from the root of c1.
   c1 : The original tree, indexed and hashed.
   c2 : Copied tree, fully committed, i.e. indexed and hashed.

   c1 and c2 must share the same shape.
*)
let find_copied_nodes new_nodes c1 c2 =
    (* Direct children *)
  let children c = 
    let c, v = Cursor.view c in
    match v with
    | Node.Leaf _ | Bud (None, _, _) -> []
    | Bud (Some _, _, _) -> [from_Some (from_Ok (Cursor.go_below_bud c))]
    | Internal (_, _, _, _) ->
        let c1 = from_Ok @@ Cursor.go_side Left c in
        let c2 = from_Ok @@ Cursor.go_side Right c in
        [c1; c2]
    | Extender (_, _, _, _) -> [from_Ok (Cursor.go_down_extender c)]
  in
  (* simultaneous traversal of tree *)
  let rec loop acc cs1 cs2 = match cs1, cs2 with
    | [], [] -> acc
    | _::_, [] | [], _::_ -> assert false (* trees have different shapes! *)
    | c1::cs1, c2::cs2 ->
        let c1, v1 = Cursor.view c1 in
        let c2, v2 = Cursor.view c2 in
        (* check the shape *)
        begin match v1, v2 with
        | Node.Leaf _, Leaf _ -> ()
        | Bud (None,_,_), Bud (None,_,_) -> ()
        | Bud (Some _,_,_), Bud (Some _,_,_) -> ()
        | Internal _, Internal _ -> ()
        | Extender _, Extender _ -> ()
        | _ -> assert false
        end;
        let i = from_Some @@ Node.index_of_view v1 in
        if not @@ Index.Map.mem i new_nodes then 
          (* do not go below *)
          loop acc cs1 cs2
        else begin
          loop (Index.Map.add i (Node.View v2) acc) 
            (children c1 @ cs1) (children c2 @ cs2)
        end
  in
  loop Index.Map.empty [c1] [c2]

(* Get the nodes reachable from the cursor, excluding the small leaves *)
let get_nodes c =
  let context = Cursor.context c in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in
  Format.eprintf "Scanning nodes from #%d@."
    (Index.to_int @@ from_Some @@ Cursor.index c);
  Cursor.fold ~init:Index.Set.empty c @@ fun new_nodes c ->
    let _, v = Cursor.view c in
    let i = from_Some @@ Node.index_of_view v in
    match v with
    | Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
        `Continue, new_nodes
    | _ ->
        `Continue, (Index.Set.add i new_nodes)

(* Restrict the domain of [past_nodes] to those in [is] *)
let filter_past_nodes is = Index.Map.filter (fun i _ -> Index.Set.mem i is)

(* Find out which node [n] was copied to *)
let get_copied_node ctxt copy_cache n =
  let hashcons_max_leaf_size = (Hashcons.config ctxt.Context.hashcons).max_leaf_size in
  let open Node in
  match view ctxt n with
  | Leaf (v, _, h) when Value.length v <= hashcons_max_leaf_size ->
      (* small leaves, handled by the cache system *)
      View (_Leaf (v, Not_Indexed, h))
  | v -> 
      let i = from_Some @@ Node.index n in
      match Index.Map.find i copy_cache with
      | exception Not_found -> 
          Format.eprintf "Node #%d %s is not found in copy_cache@." (Index.to_int i) (get_kind v);
          assert false
      | n' -> n'

(* copy a view.  [copy_cache] is used for the children. *)
let copy_view ctxt copy_cache old_v = 
  let open Node in
  match old_v with
  | Leaf (v, _i, h) -> _Leaf (v, Not_Indexed, h)
  | Internal (n1, n2, _i, h) ->
      let n1' = get_copied_node ctxt copy_cache n1 in
      let n2' = get_copied_node ctxt copy_cache n2 in
      _Internal (n1', n2', Not_Indexed, h)
  | Bud (None, _i, h) -> _Bud (None, Not_Indexed, h)
  | Bud (Some n, _i, h) ->
      let n' = get_copied_node ctxt copy_cache n in
      _Bud (Some n', Not_Indexed, h)
  | Extender (seg, n, _i, h) ->
      let n' = get_copied_node ctxt copy_cache n in
      _Extender (seg, n', Not_Indexed, h)

(* Copy nodes in [new_nodes] and accumulate the new nodes to [copy_cache] *)
let copy_new_nodes ctxt copy_cache new_nodes =
  (* Must visit from the eldest *)
  let new_nodes = 
    List.sort (fun (i1, _) (i2, _) -> Index.compare i1 i2) 
    @@ Index.Map.bindings new_nodes
  in
  List.fold_left (fun copy_cache (i, v) -> 
      let v' = copy_view ctxt copy_cache v in 
      Index.Map.add i (Node.View v') copy_cache) copy_cache new_nodes

let copy_tree past_nodes root_cursor =
  let Cursor.Cursor (_, _, ctxt) = root_cursor in
  let root_index = from_Some @@ Cursor.index root_cursor in
  let new_nodes, frontier = find_new_nodes past_nodes root_cursor in
  let copy_cache = copy_new_nodes ctxt frontier new_nodes in
  (new_nodes, 
   try Index.Map.find root_index copy_cache with Not_found -> assert false)

let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~mode:Storage.Reader path1 in

  let path2 = Sys.argv.(2) in
  let vc2 = Vc.create path2 in

  let bud_cache = Bud_cache.empty () in

  let ctxt2 = Vc.context vc2 in
  let _ = Storage.new_indices ctxt2.storage 1000_000 in

  let roots = Vc.roots vc1 in

  let ncells = Index.to_int @@ Storage.get_current_length (Vc.context vc1).Context.storage in

  let t1 = Unix.gettimeofday () in

  let report i =
    let i = Index.to_int i in
    let t2 = Unix.gettimeofday () in
    let diff = t2 -. t1 in
    let ratio = float i /. float (max 1 ncells) in
    Format.eprintf "Report: index %d, ratio: %.02f, time: %.0f, eta: %.0f@."
      i
      (ratio *. 100.0)
      diff
      (diff /. ratio *. (1. -. ratio))
  in

  let threshold = 10_000_000 in

  let rec f = function
    | [] -> ()
    | (e, past_nodes, npast_nodes) :: jobs ->
        let root_index  = e.Roots.index in
        let root_hash   =  (from_Some @@ Roots.find_by_index roots root_index).Roots.hash in
        let vcc = from_Some @@ Vc.checkout vc1 root_hash in
        let root_cursor, v = Cursor.view vcc in
        let plebeia_hash = from_Some @@ Node.hash_of_view v in

        Format.eprintf "%s: copying from %d@." (Roots.RootHash.to_hex_string root_hash) (Index.to_int root_index);

        (* copied only in memory *)

        let new_nodes, new_root = copy_tree past_nodes root_cursor in
        let new_n = Index.Map.cardinal new_nodes in
        Format.eprintf "%s: copied %d new nodes in memory@." (Roots.RootHash.to_hex_string root_hash) new_n;

        let new_root_cursor = Cursor._Cursor (Cursor._Top, new_root, Vc.context vc2) in

        (* commit to the disk *)

        let (new_root_cursor_copied, new_plebeia_hash), time = 
          let parent_hash = match e.Roots.parent with
            | None -> None
            | Some parent_i -> 
                let { Roots.hash = h ; _ } = from_Some @@ Roots.find_by_index roots parent_i in
                Some h
          in
          with_time (fun () -> 
              let res = Vc.commit bud_cache vc2 ~parent:parent_hash ~meta:e.Roots.meta ~hash_override:e.Roots.hash new_root_cursor in
              Bud_cache.shrink 100_000 bud_cache;
              res)
        in
        Format.eprintf "%s: commited %d nodes in %f sec@." (Roots.RootHash.to_hex_string root_hash) new_n time;

        if plebeia_hash <> new_plebeia_hash then begin
          Format.eprintf "Hash inconsistency!@.";
          assert false
        end;

        (* copy_cache is neither cached nor indexed.  
           We must retrieve cached+indexed versions *)
        let copied_and_committed = 
          find_copied_nodes new_nodes root_cursor new_root_cursor_copied
        in 
        Format.eprintf "%s: %d nodes are committed@." (Roots.RootHash.to_hex_string root_hash) (Index.Map.cardinal copied_and_committed);
        assert (Index.Map.cardinal copied_and_committed = new_n);

        report e.Roots.index;

        (* accumulate the jobs *)

        let past_nodes, npast_nodes =
          let npast_nodes = new_n + npast_nodes in
          let past_nodes =
            Index.Map.union (fun i _n1 _n2 -> 
                (* The intersection of copy_cache and past_nodes
                    must be the frontier *)
                failwithf "past_nodes merge: %d" (Index.to_int i))
              copied_and_committed past_nodes 
          in
          if npast_nodes > threshold then begin
            (* rescan the tree and refresh *)
            Format.eprintf "Shrinking past_nodes from %d...@." npast_nodes;
            let vcc = from_Some @@ Vc.checkout vc1 root_hash in
            let is = get_nodes vcc in
            let past_nodes = filter_past_nodes is past_nodes in
            let npast_nodes = Index.Map.cardinal past_nodes in
            Format.eprintf "Shrinked past_nodes to %d...@." npast_nodes;
            (past_nodes, npast_nodes)
          end else (past_nodes, npast_nodes)
        in

        f @@ List.map (fun e -> (e, past_nodes, npast_nodes)) (Roots.children roots e) @ jobs
  in

  f @@ List.map (fun e -> (e, Index.Map.empty, 0)) @@ Roots.genesis roots;
  
  let t2 = Unix.gettimeofday () in
  Format.eprintf "Copied %d cells in %f secs@." ncells (t2 -. t1)
