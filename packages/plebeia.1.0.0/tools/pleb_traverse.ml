open Plebeia.Internal
open Utils.Open

let (//) = Filename.concat

module IS = Set.Make(struct 
    type t = Index.t 
    let compare = compare 
  end) 

(*
   Suppose we have 2 root hashes, rh1, rh2, where rh2's parent is rh1.
   Let top1 and top2 are the top nodes of rh1 and rh2 respectively.

   A node n which is reachable from top2 is:
   
   * If the index of n is newer than the index of top1, n is unreachable
     from top1.
   
   * If the index of n is older than the index of top1, 
                                                        
       * If n is a small leaf which is cached, n may be unreachable from top1.
       * Otherwise, n is always reachable from top1.

   Using the above property, we can reduce the memory usage.
*)

let find_new_nodes vc past_nodes rh =
  let entry = from_Some @@ Roots.find (Vc.roots vc) rh in
  let parent_i = match entry.parent with Some i -> i | None -> Index.of_int 0 in
                   
  let c = from_Some @@ Vc.checkout vc rh in

  let context = Vc.context vc in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in

  Cursor.fold ~init:IS.empty c (fun new_nodes c ->
      let _, v = Cursor.view c in
      match v with
      | Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
          (* This is a small leaf *)
          `Continue, new_nodes
      | _ ->
          let i = from_Some @@ Node.index_of_view v in
          if i > parent_i then begin
            (* This is a new node, which cannot appear in past_nodes *)
            assert (not @@ IS.mem i past_nodes);
            if IS.mem i new_nodes then begin
              (* quite surprising to have shared node in one commit *)
              Format.eprintf "Shared node found in one commit! %d@." (Index.to_int i)
            end;
            let new_nodes = IS.add i new_nodes in
            `Continue, new_nodes
          end else begin
            assert (IS.mem i past_nodes);
            `Continue, new_nodes
          end)

let get_non_small_nodes c = 
  let context = Cursor.context c in
  let hashcons_max_leaf_size = (Hashcons.config context.Context.hashcons).max_leaf_size in
  Cursor.fold ~init:IS.empty c (fun new_nodes c ->
      let _, v = Cursor.view c in
      let i = from_Some @@ Node.index_of_view v in
      match v with
      | Leaf (v, _, _) when Value.length v <= hashcons_max_leaf_size ->
          (* This is a small leaf *)
          `Continue, new_nodes
      | _ ->
          let new_nodes = IS.add i new_nodes in
          `Continue, new_nodes)
  
let () =
  let path1 = Sys.argv.(1) in
  let vc1 = Vc.open_ ~mode:Storage.Reader path1 in
  (* let path2 = Sys.argv.(2) in *)
  (* let _vc2 = Vc.create path2 in *)
  let roots = Vc.roots vc1 in

  let cells = Index.to_int @@ Storage.get_current_length (Vc.context vc1).Context.storage in

  let t1 = Unix.gettimeofday () in

  (* past node table *)
  let past_nodes_tbl = Hashtbl.create 0 in

  let report i =
    let i = Index.to_int i in
    let t2 = Unix.gettimeofday () in
    let ratio = float i /. float cells in
    Format.eprintf "Report: index %d, %.02f, %.0f, %.0f@."
      i
      (ratio *. 100.0)
      (t2 -. t1)
      ((t2 -. t1) /. ratio)
  in

  let f e () = 
    let { Roots.hash = h ; _ } = from_Some @@ Roots.find_by_index roots e.Roots.index in
    let (past_nodes, n, threshold) = match e.Roots.parent with
      | None -> (IS.empty, 0, 10000)
      | Some parent_i -> 
          let (refc, past_nodes, n, threshold) = Hashtbl.find past_nodes_tbl parent_i in
          if refc <= 1 then begin
            Hashtbl.remove past_nodes_tbl parent_i;
            Format.eprintf "Forgot %d from past_node_tbl. %d entries in the table@." 
              (Index.to_int parent_i)
              (Hashtbl.length past_nodes_tbl);
          end else Hashtbl.replace past_nodes_tbl parent_i (refc-1, past_nodes, n, threshold);
          (past_nodes, n, threshold)
    in
    let new_nodes = find_new_nodes vc1 past_nodes h in
    let new_n = IS.cardinal new_nodes in
    Format.eprintf "%s: %d new nodes@." (Roots.RootHash.to_hex_string h) new_n;
    let n = new_n + n in
    let past_nodes, n, threshold = 
      (* if nchildren is 0, no point to calculate *)
      if n > threshold then
        (* rescan the tree and refresh *)
        let c = from_Some @@ Vc.checkout vc1 h in
        let past_nodes = get_non_small_nodes c in
        let n = IS.cardinal past_nodes in
        report e.Roots.index;
        (past_nodes, n, n * 8)
      else
        (IS.union past_nodes new_nodes, n, threshold) 
    in
    let nchildren = List.length @@ Roots.children roots e in
    if nchildren > 0 then begin
      Format.eprintf "Adding idx: %d  #n: %d  threshold #n: %d@." (Index.to_int e.Roots.index) n threshold;
      Hashtbl.replace past_nodes_tbl e.Roots.index (nchildren, past_nodes, n, threshold)
    end
  in

  Roots.fold_breadth_first f roots ();
  let t2 = Unix.gettimeofday () in
  Format.eprintf "Traversed %d cells in %f secs@." cells (t2 -. t1)


