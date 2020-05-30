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
open Utils.Open
open Node
open Segment

let debug = false

type t = 
  | ModLeaf     of node * node * Segs.t (* leaf modification *)
  | CleanBud    of node * Segs.t (* bud node in the old at segs is emptied.  The node is the bud, and itself is not deleted *)
  | Del         of node * Segs.t (* node in the old at segs is removed *)
  | CopyBud     of Segs.t * node * Segs.t (* node is copied at segs *)
  | NewEmptyBud of node * Segs.t (* empty bud node is created at segs *)
  | CopyLeaf    of Segs.t * node * Segs.t (* old leaf node was copied to segs *)
  | NewLeaf     of node * Segs.t (* new leaf node created *)
  | SmallLeaf   of node * Segs.t (* new small leaf node created *)

let pp ppf x = 
  let f fmt = Format.fprintf ppf fmt in
  match x with
  | NewLeaf   (_n, segs)       -> f "NewLeaf %s@."     @@ Segs.to_string segs
  | SmallLeaf (_n, segs)       -> f "SmallLeaf %s@."   @@ Segs.to_string segs
  | Del       (_n, segs)       -> f "Del %s@."         @@ Segs.to_string segs
  | CleanBud  (_n, segs)       -> f "CleanBud %s@."    @@ Segs.to_string segs
  | ModLeaf   (_n1, _n2, segs) -> f "ModLeaf %s@."     @@ Segs.to_string segs
  | CopyLeaf  (osegs, _, segs) -> f "CopyLeaf %s => %s@." (Segs.to_string osegs) (Segs.to_string segs)
  | CopyBud   (osegs, _, segs) -> f "CopyBud %s => %s@." (Segs.to_string osegs) (Segs.to_string segs)
  | NewEmptyBud  (_, segs)     -> f "NewEmptyBud %s@." @@ Segs.to_string segs

(* convert the segment based diffs to leaf+bud based *)
let enrich context n1 diffs =
  (* Search the copy source *)
  let search = Search.from @@ Cursor._Cursor (Cursor._Top, n1, context) in

  let old_max_index = 
    match index_of_view @@ view context n1 with
    | None -> Index.zero (* genesis *)
    | Some i -> i
  in
  let is_old n = 
    (from_Some @@ index_of_view @@ view context n) <= old_max_index 
  in

  fst 
  @@ List.fold_left (fun (acc, search) -> function
      | Diff.ModLeaf (n1, n2, segs) -> 
          ModLeaf (n1, n2, segs) :: acc,
          search
      | CleanBud (n, segs) ->
          CleanBud (n, segs) :: acc,
          search

      | Del (n, segs) ->
          (* Get leaves and buds reachable from [n].  
              Scan stops at buds.
          *)
          let segns = Node_tools.ls context n in
          List.rev_append (List.rev_map (fun (seg,n) -> 
              Del (n, Segs.append_seg segs seg)) segns) acc,
          search
      | Add (n, segs) ->
          let rec f (acc, search) frontiers = match frontiers with
            | [] -> acc, search
            | (n, segs) :: frontiers ->
                let v = view context n in
                match v with
                | Internal (n1, n2, _, _) -> 
                    f (acc, search) ((n1, Segs.add_side segs Left)
                                     :: (n2, Segs.add_side segs Right)
                                     :: frontiers)
                | Extender (seg, n, _, _) ->
                    f (acc, search) ((n, Segs.append_seg segs seg) :: frontiers)

                | Leaf (v, _, _) when Value.length v <= (Hashcons.config context.hashcons).max_leaf_size (* XXX should have an API *) (* XXX zero sized leaf? *) -> 
                    f (SmallLeaf (n, segs)::acc, search) frontiers

                | Leaf _ ->
                    if is_old n then begin
                      match Search.run search n with
                      | Ok (Some osegs, search) ->
                          f (CopyLeaf (osegs, n, segs)::acc,
                             search) frontiers
                      | Ok (None, _) -> assert false (* XXX *)
                      | Error _ -> assert false (* XXX *)
                    end else
                      f (NewLeaf (n, segs)::acc, search) frontiers

                | Bud (None, _, _) -> 
                    f (NewEmptyBud (n, segs)::acc, search) frontiers

                | Bud (Some n', _, _) -> 
                    if is_old n then begin
                      match Search.run search n with
                      | Ok (Some osegs, search) ->
                          f (CopyBud (osegs, n, segs)::acc, search) frontiers
                      | Ok (None, _) -> assert false (* XXX *)
                      | Error _ -> assert false (* XXX *)
                    end else
                      f (acc, search) ((n', Segs.push_bud segs) :: frontiers)
          in
          f (acc, search) [(n, segs)]) ([], search) diffs

(* each directory removal is converted to file removals 

   It is "pseudo" since information of CleanBud is lost. 
*)
let expand_bud_deletions context d = match d with
  | ModLeaf _ 
  | CopyBud _ 
  | NewEmptyBud _
  | CopyLeaf _
  | NewLeaf _
  | SmallLeaf _ -> [d]
  | Del (n, segs)
  | CleanBud (n, segs) ->
      List.rev_map (fun (segs', n) -> 
            Del (n, 
                 List.fold_left (fun segs s ->
                     Segs.append_seg (Segs.push_bud segs) s) segs segs'))
      @@ Node_tools.ls_rec context n

let apply c diff =
  let Cursor.Cursor (_, _, context) = c in
  let from_Ok mes = function
    | Ok v -> v
    | Error e -> failwith (Error.show e ^ " : " ^ mes)
  in
  if debug then Log.debug "%a" pp diff;
  match diff with
  | NewLeaf (n, segs) ->
      begin match view context n with
        | Leaf (v, _, _) ->
            from_Ok "leaf" @@ Deep.insert c (Segs.finalize segs) v
        | _ -> assert false
      end

  | SmallLeaf (n, segs) ->
      begin match view context n with
        | Leaf (v, _, _) ->
            from_Ok "leaf" @@ Deep.insert c (Segs.finalize segs) v
        | _ -> assert false
      end

  | Del (_n, segs) ->
      from_Ok "delete" @@ Deep.delete c (Segs.finalize segs)

  | CleanBud (_n, segs) ->
      let c = from_Ok "delete" @@ Deep.delete c (Segs.finalize segs) in
      let c = from_Ok "create_subtree" @@ Deep.create_subtree ~create_subtrees:false c (Segs.finalize segs) in
      c

  | ModLeaf (_n1, n2, segs) ->
      begin match view context n2 with
      | Leaf (v, _, _) ->
          begin match 
              Deep.upsert c (Segs.finalize segs) v
            with
            | Ok c -> c
            | Error _ ->
                match Deep.get c (Segs.finalize segs) with
                | Ok _ -> assert false
                | Error (Cursor.Access (Middle_of_extender (c, _share, _other, must_be_empty))) when Segment.is_empty must_be_empty -> 
                    Log.fatal "ACC %s" (Segs.to_string segs);
                    let segs = List.fold_left Segs.append_seg Segs.empty (Cursor.segs_of_cursor c) in 
                    Log.fatal "ACC %s" (Segs.to_string segs);

                    assert false
                | Error _ -> assert false
          end
      | _ -> assert false
      end

  | NewEmptyBud (_n, segs) -> 
      from_Ok "new empty bud" @@ Deep.create_subtree ~create_subtrees:true c (Segs.finalize segs)

  | CopyLeaf (_osegs, n, segs) -> 
      from_Ok "copyLeaf" @@ Deep.link n c (Segs.finalize segs)

  | CopyBud (_osegs, n, segs) -> 
      from_Ok "copyBud" @@ Deep.link n c (Segs.finalize segs)

(* Apply the diffs against the original and see the final hashes are the same *)
let check diffs context n1 n2 =
  let c = Cursor._Cursor (Cursor._Top, n1, context) in 
  let c' = List.fold_left apply c diffs in
  let h = from_Some @@ Node.hash_of_view @@ Node.view context n2 in
  let _, nh' = Cursor_hash.compute c' in
  let h' = Node_hash.prefix nh' in
  let Cursor (_, n2', _) = c' in

  if h <> h' then begin
    prerr_endline "hash conflict!";
    
    let diffs = Diff.diff context n2 n2' in

    List.iter (fun x -> Log.fatal "%a" Diff.pp x) diffs
  end;
  assert (h = h')

let diff context n1 n2 =
  enrich context n1 @@ Diff.diff context n1 n2
