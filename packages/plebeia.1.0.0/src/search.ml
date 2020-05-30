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
(* Search by index.
*)

open Utils.Open
open Node
open Segment
open Result

type t = 
  { cache    : Segs.t Index.Map.t
  ; frontier : (Cursor.t * Segs.t) list
  ; context  : Context.t
  }

let run t n = match Node.view t.context n with
  | Internal _ | Extender _ -> Error "Must be a bud or leaf"
  | (Leaf _ | Bud _ as v) ->
      match Node.index_of_view v with
      | None -> Error "View is not indexed"
      | Some i ->
          try
            match Index.Map.find_opt i t.cache with
            | Some segs -> Ok (Some segs, t)
            | None ->
                let rec loop cache skipped = function
                  | [] -> Ok (None, { t with cache; frontier= skipped })
                  | (c,(segs:Segs.t))::cs ->
                      let c, v = Cursor.view c in
                      let j = match Cursor.index c with
                        | None -> failwith "not indexed"
                        | Some j -> j
                      in
                      let cache = match v with
                        | Bud _ | Leaf _ ->
                            (* Add only if bud and leaf.
                               Otherwise it easily runs out of memory.
                            *)
                            Index.Map.add j segs cache
                        | _ -> cache
                      in
                      let children = match v with
                        | Bud (None, _, _) | Leaf _ -> []
                        | Bud _ -> 
                            let c = from_Some @@ from_Ok @@ Cursor.go_below_bud c in
                            [c, Segs.push_bud segs]
                        | Internal _ ->
                            let cl = from_Ok @@ Cursor.go_side Left c in
                            let segsl = Segs.add_side segs Left in
                            let cr = from_Ok @@ Cursor.go_side Right c in
                            let segsr = Segs.add_side segs Right in
                            [cl,segsl; cr,segsr]
                        | Extender (seg, _ , _, _) -> 
                            let c = from_Ok @@ Cursor.go_down_extender c in
                            let segs = Segs.append_seg segs seg in
                            [c,segs]
                      in
                      if i = j then  Ok (Some segs, { t with cache; frontier= List.rev_append children @@ List.rev_append skipped cs })
                      else if i > j then 
                        (* we will not find i under j *)
                        loop cache (List.rev_append children skipped) cs
                      else 
                        (* we may find i under j *)
                        loop cache skipped (List.rev_append children cs)
                in
                loop t.cache [] t.frontier
          with
          | Failure s -> Error s

let from (Cursor.Cursor (_, _, ctxt) as c) = 
  { cache= Index.Map.empty
  ; frontier= [c, Segs.empty]
  ; context= ctxt
  }
    
