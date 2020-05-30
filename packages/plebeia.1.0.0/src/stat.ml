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
(* Mutable stat *)
type t = 
  { mutable loaded_nodes        : int
  ; mutable written_leaves      : int
  ; mutable written_empty_buds  : int
  ; mutable written_buds        : int
  ; mutable written_internals   : int
  ; mutable written_extenders   : int
  ; mutable written_big_extenders : int
  ; mutable written_links       : int
  ; mutable written_leaf_sizes  : (int, int) Hashtbl.t (* actual leaves written to disk *)
  ; mutable committed_leaf_sizes : (int, int) Hashtbl.t (* including hashcons'ed leaves *)
  }

let create () =
  { loaded_nodes        = 0
  ; written_leaves      = 0
  ; written_empty_buds  = 0
  ; written_buds        = 0
  ; written_internals   = 0
  ; written_extenders   = 0
  ; written_big_extenders = 0
  ; written_links       = 0
  ; written_leaf_sizes  = Hashtbl.create 0
  ; committed_leaf_sizes = Hashtbl.create 0
  }

let pp ppf t =
  let f fmt = Format.fprintf ppf fmt in
  f "Loaded nodes          : %d@." t.loaded_nodes;
  f "Written leaves        : %d@." t.written_leaves;
  f "Written empty buds    : %d@." t.written_empty_buds;
  f "Written buds          : %d@." t.written_buds;
  f "Written internals     : %d@." t.written_internals;
  f "Written extenders     : %d@." t.written_extenders;
  f "Written big extenders : %d@." t.written_big_extenders;
  f "Written links         : %d@." t.written_links;
  f "Written leaf data:@.";
  let xs = List.sort compare @@ Hashtbl.fold (fun k v st -> (k,v)::st) t.written_leaf_sizes [] in
  List.iter (fun (sz,n) -> 
      let n' = Hashtbl.find t.committed_leaf_sizes sz in
      if n <> n' then
        f "%d : %d (hashcons: x%.2f, saved %.2fMB) @." 
          sz n (float n' /. float n) (float (sz * (n' - n)) /. 1000000.0)
      else
        f "%d : %d@." sz n) xs

let incr_loaded_nodes       t = t.loaded_nodes <- t.loaded_nodes + 1
let incr_written_leaves     t = t.written_leaves <- t.written_leaves + 1
let incr_written_empty_buds t = t.written_empty_buds <- t.written_empty_buds + 1
let incr_written_buds       t = t.written_buds <- t.written_buds + 1
let incr_written_internals  t = t.written_internals <- t.written_internals + 1
let incr_written_extenders  t = t.written_extenders <- t.written_extenders + 1
let incr_written_big_extenders  t = t.written_big_extenders <- t.written_big_extenders + 1
let incr_written_links      t = t.written_links <- t.written_links + 1
let incr_written_leaf_sizes t s = 
  let tbl = t.written_leaf_sizes in
  match Hashtbl.find_opt tbl s with
  | None -> Hashtbl.add tbl s 1
  | Some x -> Hashtbl.replace tbl s (x + 1)
let incr_committed_leaf_sizes t s = 
  let tbl = t.committed_leaf_sizes in
  match Hashtbl.find_opt tbl s with
  | None -> Hashtbl.add tbl s 1
  | Some x -> Hashtbl.replace tbl s (x + 1)
