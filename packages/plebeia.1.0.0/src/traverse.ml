(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
(*
   Suppose we have 2 root hashes, rh1, rh2, where rh2 is a decendant of rh1.
   Let top1 and top2 are the top nodes of rh1 and rh2 respectively.

   A node n which is reachable from top2 is:
   
   * If the index of n is newer than the index of top1, n is unreachable
     from top1.
   
   * If the index of n is older than the index of top1, 
                                                        
       * If n is a small leaf which is cached, n may be unreachable from top1.
       * Otherwise, n is always reachable from top1.

   Using the above property, we can reduce the memory usage.
*)

open Utils.Open

(* Traverse the nodes from [c] and returns the new nodes not in [past_nodes].
   It also returns the "frontier", the first reachable nodes from [c] which 
   were in [past_nodes].

   This function assumes the following property: commits are atomic, i.e., 
   any node reachable from [c] older than the newest node in [past_nodes] 
   must be in [past_nodes].
   
   * [past_nodes]:  The past nodes.
*)
let find_new_nodes e c st g =
  let Cursor.Cursor (_, _, context) = c in
  let past_max_i = match e.Roots.parent with
    | None -> Index.zero (* zero is used for header *)
    | Some i -> i
  in
  Cursor.fold ~init:(Index.Map.empty, st) c @@ fun (new_nodes, st) c ->
    let i = from_Some @@ Cursor.index c in
    let is_new = past_max_i < i in
    let _c, v = Cursor.view c in
    match v with
    | Node.Leaf (v, _, _) when Value.length v <= (Hashcons.config context.hashcons).max_leaf_size -> (* XXX think about zero sized leaf *)
        (* Small leaves. *)
        let st = g st e.Roots.index ((if is_new then `NewSmallLeaf else `Cached), c) in
        `Continue, (new_nodes, st)
    | _ ->
      if is_new then 
        if Index.Map.mem i new_nodes then
          (* Seeing the same new node more than once *)
          `Up, (new_nodes, st)
        else
          let st = g st e.Roots.index (`New, c) in
          `Continue, (Index.Map.add i v new_nodes, st)
      else
        (* This should be an old node.
           Stop the traversal here.
        *)
        let st = g st e.Roots.index (`Old, c) in
        `Up, (new_nodes, st)

let f vc g h st =
  let roots = Vc.roots vc in

  let ncells = Index.to_int @@ Storage.get_current_length (Vc.context vc).Context.storage in

  let t1 = Unix.gettimeofday () in

  let report i =
    let i = Index.to_int i in
    let t2 = Unix.gettimeofday () in
    let diff = t2 -. t1 in
    let ratio = float i /. float (max 1 ncells) in
    Log.notice "Report: index %d, ratio: %.02f%%, time: %.0f, eta: %.0f"
      i
      (ratio *. 100.0)
      diff
      (diff /. ratio *. (1. -. ratio))
  in

  let rec f st = function
    | [] -> st
    | e :: jobs ->
        let root_index  = e.Roots.index in
        let root_hash   = (from_Some @@ Roots.find_by_index roots root_index).Roots.hash in
        let root_cursor, v = Cursor.view @@ from_Some @@ Vc.checkout vc root_hash in
        let _plebeia_hash = from_Some @@ Node.hash_of_view v in

        Log.notice "%s: index %d" (Roots.RootHash.to_hex_string root_hash) (Index.to_int root_index);

        let new_nodes, st = find_new_nodes e root_cursor st g in
        let new_n = Index.Map.cardinal new_nodes in
        Log.notice "%s: %d new nodes" (Roots.RootHash.to_hex_string root_hash) new_n;

        report e.Roots.index;

        (* accumulate the jobs *)

        let st = h st e in

        f st @@ (Roots.children roots e) @ jobs
  in

  let st = f st @@ Roots.genesis roots in
  
  let t2 = Unix.gettimeofday () in
  Log.notice "Traversed %d cells in %f secs" ncells (t2 -. t1);
  st
