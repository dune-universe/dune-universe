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
open Segment
open Node

let ls context n = 
  let rec f acc frontiers = match frontiers with
    | [] -> acc
    | (n, rev_seg)::frontiers ->
        let v = view context n in
        let n = View v in
        match v with
        | Leaf _ | Bud _ -> 
            f ((Segment.of_sides @@ List.rev rev_seg, n) :: acc) frontiers
        | Internal (n1, n2, _, _) -> 
            f acc ((n1,Left::rev_seg)::(n2,Right::rev_seg)::frontiers)
        | Extender (seg, n, _, _) ->
            f acc ((n,  List.rev_append (to_sides seg) rev_seg) :: frontiers) 
  in
  f [] [n, []]

let ls_rec context n = match Node.view context n with
  | Internal _ | Extender _ -> assert false
  | Leaf _ -> [([], n)]
  | Bud (None, _, _) -> []
  | Bud (Some n, _, _) ->
      let rec f res = function
        | [] -> res
        | (segs, n) :: segsns ->
            match view context n with
            | Internal (n1, n2, _, _) ->
                f res ((Segs.add_side segs Left, n1)
                             :: (Segs.add_side segs Right, n2)
                             :: segsns)
            | Extender (seg, n, _, _) ->
                f res ((Segs.append_seg segs seg, n) :: segsns)
            | Leaf _ -> 
                f ((Segs.finalize segs, n) :: res) segsns
            | Bud (Some n, _, _) -> 
                f res ((Segs.push_bud segs, n) :: segsns)
            | Bud (None, _, _) -> 
                f res segsns
      in
      f [] [(Segs.empty, n)] 

let equal context n1 n2 =
  let rec aux = function
    | [] -> Ok ()
    | (n1,n2)::rest ->
        match n1, n2 with
        | Disk (i1, ew1), Disk (i2, ew2) when i1 = i2 && ew1 = ew2 -> aux rest
        | Disk _, Disk _ -> Error (n1,n2)
        | Disk (i, ew), n2 ->
            let n1 = View (load_node context i ew) in
            aux @@ (n1,n2)::rest
        | n1, Disk (i, ew) ->
            let n2 = View (load_node context i ew) in
            aux @@ (n1,n2)::rest
        | View v1, View v2 ->
            match v1, v2 with
            | Internal (n11, n12, _, _), Internal (n21, n22, _, _) ->
                aux @@ (n11,n21)::(n12,n22)::rest
            | Bud (None, _, _), Bud (None, _, _) -> aux rest
            | Bud (Some n1, _, _), Bud (Some n2, _, _) -> aux @@ (n1,n2) :: rest
            | Leaf (v1, _, _), Leaf (v2, _, _) when v1 = v2 -> aux rest
            | Extender (seg1, n1, _, _), Extender (seg2, n2, _, _) when Segment.equal seg1 seg2 ->
                aux @@ (n1,n2)::rest
            | _ -> Error (n1,n2)
  in
  aux [(n1, n2)]

let count_cells ~upto context n =
  (* cannot count links *)
  let rec loop visited i ns = 
    if i > upto then `GE i
    else match ns with
      | [] -> `EQ i
      | n::ns -> 
          let visit visited n = 
            match Node.view context n with
            | Internal (n1, n2, _, _) -> loop visited (i+1) (n1::n2::ns)
            | Extender (_, n, _, _) -> loop visited (i+1) (n::ns)
            | Bud (None, _, _) -> loop visited (i+1) ns
            | Bud (Some n, _, _) -> loop visited (i+1) (n::ns)
            | Leaf (v, _, _) ->
                match Value.length v with
                | 0 -> loop visited i ns (* it does not use any cell *)
                | sz when sz <= 32 -> loop visited (i+2) ns
                | sz when sz <= 64 -> loop visited (i+3) ns
                | sz -> (* hard to quickly obtain the exact cells used for big leaves *)
                    loop visited (i + (sz + 6 + 31) / 32) ns
          in
          match Node.index n with
          | Some id when Index.Set.mem id visited -> loop visited i ns
          | Some id -> visit (Index.Set.add id visited) n
          | None -> visit visited n
  in
  loop Index.Set.empty 0 [n]
