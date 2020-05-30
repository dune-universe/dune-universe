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
(* XXX Should be moved to the library *)
open Utils.Open
open Node
open Segment

(* segment based diff *)

(* strings are for debugging purpose *)
type t = 
  | Add of node * Segs.t
  | Del of node * Segs.t
  | CleanBud of node * Segs.t
  | ModLeaf of node * node * Segs.t

let pp ppf = 
  let open Format in
  let f fmt = fprintf ppf fmt in
  let i n = Index.to_int @@ from_Some @@ Node.index n in
  function
  | Add (n, segs) -> 
      f "Add %d %s" (i n) (Segs.to_string segs)
  | Del (n, segs) ->
      f "Del %d %s" (i n) (Segs.to_string segs)
  | CleanBud (n, segs) ->
      f "CleanBud %d %s" (i n) (Segs.to_string segs)
  | ModLeaf (_n1, n2, segs) -> 
      f "ModLeaf %d %s" (i n2) (Segs.to_string segs)

let diff context n1 n2 =
  let rec loop diff frontiers = 
    match frontiers with
    | [] -> diff
    | (n1,n2,segs)::frontiers ->
        let loop' new_diffs new_frontiers =
          loop (new_diffs @ diff) (new_frontiers @ frontiers)
        in
        let v1 = view context n1 in
        let v2 = view context n2 in
        let i1 = index_of_view v1 in
        let i2 = index_of_view v2 in
        let h1 = hash_of_view v1 in
        let h2 = hash_of_view v2 in
        (* A Bud has the same has as its child.  We also need to check the form of the node. *)
        let hash_same = match h1, h2 with Some h1, Some h2 -> h1 = h2 | _ -> false in
        (* comparing indices are faster than comparing hashes *)
        match i1, i2 with
        | Some i1, Some i2 when i1 = i2 -> 
            (* identical *)
            loop' [] []
        | _ ->
            match v1, v2 with
            | Bud (None, _, _), Bud (None, _, _) ->
                (* really no change *)
                loop' [] []
            | Leaf (val1, _, _), Leaf (val2, _, _) when val1 = val2 ->
                (* really no change *)
                loop' [] []

            | Leaf (_, _, _), Leaf (_, _, _) ->
                loop' [ModLeaf (n1, n2, segs)] []

            | Bud (None, _, _), Bud (Some n, _, _) ->
                loop' [Add (n, Segs.push_bud segs)] []

            | Bud (Some _, _, _), Bud (None, _, _) ->
                (* Bud itself is kept. *)
                loop' [CleanBud (n1, segs)] []

            | Bud _, (Leaf _ | Internal _ | Extender _)
            | Leaf _, (Bud _ | Internal _ | Extender _)
            | (Internal _ | Extender _), (Bud _ | Leaf _) ->
                loop' [Del (n1, segs) ; Add (n2, segs)] []

            | Bud _, Bud _ when hash_same -> loop' [] []
            | Internal _, Internal _ when hash_same -> loop' [] []
            | Extender _, Extender _ when hash_same -> loop' [] []

            | Bud (Some n1, _, _), Bud (Some n2, _, _) ->
                loop' [] [(n1, n2, Segs.push_bud segs)]

            | Internal (n1l,n1r,_,_), Internal (n2l,n2r,_,_) ->
                loop' [] [ (n1l,n2l, Segs.add_side segs Left) 
                         ; (n1r,n2r, Segs.add_side segs Right)
                         ]

            | Extender (seg1, n1, _, _), Extender (seg2, n2, _, _) when equal seg1 seg2 ->
                loop' [] [ (n1, n2, Segs.append_seg segs seg1) ]

            | Extender (seg1, n1, i1, h1), Extender (seg2, n2, i2, h2) ->
                let f seg1 seg2 segs =
                  match is_empty seg1, is_empty seg2 with
                  | false, false ->
                      loop' [] [ (View (_Extender (seg1, n1, i1, h1)),
                                 View (_Extender (seg2, n2, i2, h2)),
                                 segs) ]
                  | true, true ->
                      loop' [] [ (n1, n2, segs) ]
                  | true, false ->
                      loop' [] [ (n1, View (_Extender (seg2, n2, i2, h2)),
                                 segs) ]
                  | false, true ->
                      loop' [] [ (View (_Extender (seg1, n1, i1, h1)),
                                 n2, segs) ]
                in
                begin match cut seg1, cut seg2 with
                | None, _ | _, None -> assert false
                | Some (Left, seg1), Some (Left, seg2) ->
                    let segs = Segs.add_side segs Left in
                    f seg1 seg2 segs
                | Some (Right, seg1), Some (Right, seg2) ->
                    let segs = Segs.add_side segs Right in
                    f seg1 seg2 segs
                | _ ->
                    loop' [Del (View v1, segs); 
                           Add (View v2, segs)] []
                end


            | Internal (n1l, n1r, _, _), Extender (seg, n2, i, h) ->
                begin match cut seg with
                  | None -> assert false
                  | Some (Left, seg) ->
                      loop' [Del (n1r, Segs.add_side segs Right)]
                        [ (n1l, 
                           (if is_empty seg then n2
                            else View (_Extender (seg, n2, i, h))), 
                           Segs.add_side segs Left) ]
                  | Some (Right, seg) ->
                      loop' 
                        [ Del (n1l, Segs.add_side segs Left) ]
                        [ (n1r, 
                           (if is_empty seg then n2
                            else View (_Extender (seg, n2, i, h))), 
                           Segs.add_side segs Right) ]
                end

            | Extender (seg, n1, i, h), Internal (n2l, n2r, _, _) ->
                begin match cut seg with
                  | None -> assert false
                  | Some (Left, seg) ->
                      loop' 
                        [ Add (n2r, Segs.add_side segs Right) ]
                        [ ((if is_empty seg then n1
                            else View (_Extender (seg, n1, i, h))), 
                           n2l,
                           Segs.add_side segs Left) ]
                  | Some (Right, seg) ->
                      loop' 
                        [ Add (n2l, Segs.add_side segs Left) ] 
                        [ ((if is_empty seg then n1
                            else View (_Extender (seg, n1, i, h))), 
                           n2r, 
                           Segs.add_side segs Right) ]
                end

  in
  loop [] [n1, n2, Segs.empty]
