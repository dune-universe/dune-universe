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
include Monad.Make1(struct
    type 'a t = Random.State.t -> 'a
    let bind (gen: 'a t) (f: 'a -> 'b t) st = f (gen st) st
    let return x = fun _st -> x
  end)

module RS = Random.State

let int sz : int t = fun st -> 
  RS.int st sz

let string int char : string t = fun st -> 
  let l = int st in
  String.init l (fun _ -> char st)

let list int n : 'a list t = fun st ->
  let l = int st in
  List.init l (fun _ -> n st)
  
let char : char t = int 256 >>| Char.chr

let bool : bool t = RS.bool

let segment int : Segment.t t =
  list int (bool >>| fun b -> if b then Segment.Left else Segment.Right) >>|
  Segment.of_sides

let choice : 'a t list -> 'a t = fun xs ->
  assert (xs <> []);
  int (List.length xs) >>= fun i ->
  List.nth xs i

let shuffle xs : 'a list t = fun st ->
  let a = Array.of_list xs in
  let size = Array.length a in
  for i = 0 to size - 2 do
    let pos = Random.State.int st (size - i - 1) + i in
    let x = Array.unsafe_get a pos in
    Array.unsafe_set a pos @@ Array.unsafe_get a i;
    Array.unsafe_set a i x
  done;
  Array.to_list a


let rec leaf = 
  string (int 16) char >>| fun s ->
  Node.new_leaf @@ Value.of_string s

and bud_none = return (Node.new_bud None)

and bud_some depth =
  if depth = 0 then return (Node.new_bud None)
  else
    int depth >>= fun n ->
    if n = 0 then return (Node.new_bud None)
    else begin
      let depth = depth - 1 in
      choice [extender depth; internal depth] >>| fun n ->
      begin match n with
        | Node.View (Node.Leaf _) -> assert false
        | Node.View (Node.Bud _) -> assert false
        | _ -> 
      Node.new_bud (Some n)
        end
    end

and internal depth =
  let f =
    let depth = depth - 1 in
    if depth = 0 then choice [bud_none; leaf]
    else begin
      int depth >>= fun n ->
      if n = 0 then choice [bud_none; leaf]
      else
        choice [internal depth; extender depth; bud_some depth]
    end
  in
  f >>= fun n1 ->
  f >>| fun n2 ->
  let n = Node.new_internal n1 n2 in
  match n with
  | View (Internal _) -> n
  | _ -> assert false

and extender depth =
  let depth' = max 1 depth in
  int depth' >>= fun l ->
  let l = max 1 l in
  segment (return l) >>= fun seg ->
  let depth = max 0 (depth - l) in
  (if depth = 0 then choice [bud_none; leaf]
   else
     int depth >>= fun n ->
     if n = 0 then choice [bud_none; leaf]
     else choice [internal depth; bud_some depth]) >>| fun n ->
  match Node.new_extender seg n with
  | View (Extender _) as n -> n
  | _ -> assert false

and root depth = bud_some depth
