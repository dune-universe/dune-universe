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
include Debug
include Utils
include Utils.Open

let timed f =
  let t1 = Unix.gettimeofday () in
  let res = Exn.catch f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

module RS = Random.State

let random_string st len = String.init len (fun _ -> Char.chr @@ RS.int st 256)

let random_short_segment ?length st =
  let open Random.State in
  let length =
    match length with
    | None -> int st Segment.max_length  + 1
    | Some l -> l
  in
  assert (0 < length && length <= Segment.max_short_segment_length);
  let rec f = function
    | 0 -> []
    | n -> (if bool st then Segment.Left else Segment.Right) :: f (n-1)
  in
  Segment.of_sides @@ f length

let random_segment ?length st =
  let open Random.State in
  let length =
    match length with
    | None -> int st Segment.max_length + 1
    | Some l -> l
  in
  assert (0 < length && length <= Segment.max_length);
  let rec f = function
    | 0 -> []
    | n -> (if bool st then Segment.Left else Segment.Right) :: f (n-1)
  in
  Segment.of_sides @@ f length

let random_choice st xs =
  assert (xs <> []);
  let i = RS.int st (List.length xs) in
  List.nth xs i

let shuffle st xs =
  let a = Array.of_list xs in
  let size = Array.length a in
  for i = 0 to size - 2 do
    let pos = Random.State.int st (size - i - 1) + i in
    let x = Array.unsafe_get a pos in
    Array.unsafe_set a pos @@ Array.unsafe_get a i;
    Array.unsafe_set a i x
  done;
  Array.to_list a

let tempfile = Filename.temp_file "plebeia" ".context"

let () = Log.notice "Using temp file %s ..." tempfile

let test_with_context length f =
  if Sys.file_exists tempfile then Sys.remove tempfile;
  let context = Context.create ~length tempfile in
  let res = f context in
  Context.close context;
  res

let test_with_cursor f =
  test_with_context 1000000 (fun context ->
    let cursor = Cursor.empty context in
    f cursor)

let path_of_string s = from_Some @@ Segment.of_string s

let ok_or_fail = function
  | Ok x -> x
  | Error s -> Error.raise s

let must_fail = function
  | Ok _ -> failwith "must fail"
  | Error _ -> ()

let path = path_of_string
let value = Value.of_string

open Node

(* only for test *)
let rec normalize n = match n with
  | Disk _ -> n
  | View v -> View (normalize_view v)

and normalize_view v = match v with
  | Internal (n1, n2, i, h) -> _Internal (normalize n1, normalize n2, i, h)
  | Bud (None, _, _) -> v
  | Bud (Some n, i, h) -> _Bud (Some (normalize n), i, h)
  | Leaf _ -> v
  | Extender (seg, n, i, h) -> _Extender (Segment.(of_sides @@ to_sides seg), n, i, h)

let equal_nodes n1 n2 = normalize n1 = normalize n2

let random_node st depth =
  let open Gen in
  let choice gens = fun st ->
    (random_choice st gens) st
  in
  let gen_segment length st = random_segment ~length st in
  let gen_leaf st =
    let size = RS.int st 16 in
    new_leaf @@ Value.of_string @@ random_string st size
  in
  let gen_bud_none =
    return (new_bud None)
  in
  let rec gen_node depth : node Gen.t = fun st ->
    begin
      if depth <= 0 then
        choice [gen_leaf; gen_bud_none]
      else
        let max_seg = Segment.max_length in
        choice [gen_ext max_seg depth; gen_inter max_seg depth] >>= fun node ->
        return (new_bud (Some node))
    end st
  and gen_ext max_seg depth = fun st ->
    begin
      assert (max_seg > 0);
      choice [return max_seg; return (max 1 (max_seg/2))] >>= fun len ->
      gen_segment len >>= fun seg ->
      if max_seg - len = 0 || depth <= 0 then
        gen_node (depth - 1) >>= fun node ->
        return (new_extender seg node)
      else
        choice [gen_inter (max_seg - len) depth; gen_node (depth - 1)] >>= fun node ->
        return (new_extender seg node)
    end st
  and gen_inter max_seg depth = fun st ->
    begin
      let max_seg' = max_seg - 1 in
      let gen_child =
        if max_seg' = 0 || depth <= 0 then
          gen_node (depth - 1)
        else
          choice [gen_ext max_seg' depth; gen_inter max_seg' depth;
                 gen_node (depth - 1)]
      in
      gen_child >>= fun left ->
      gen_child >>= fun right ->
      return (new_internal left right)
    end st
  in
  match gen_node depth st with
  | View (Node.Leaf _) as leaf ->
     let seg = random_segment st in
     new_bud (Some (new_extender seg leaf))
  | node -> node

(* ls . *)
let all_children context node =
  let rec aux store = function
    | [] -> store
    | (segs_rev, node) :: tail ->
       match Node.view context node with
       | Leaf _ ->
          let segment = List.rev segs_rev |> Segment.concat in
          aux ((segment, `File) :: store) tail
       | Bud (Some child, _, _) ->
          let segment = List.rev segs_rev |> Segment.concat in
          aux ((segment, `Directory child) :: store) tail
       | Bud (None, _, _) -> aux store tail
       | Extender (seg, node', _, _) ->
          aux store ((seg::segs_rev, node') :: tail)
       | Internal (l, r, _, _) ->
          aux store (
              (Segment.of_sides [Segment.Left] :: segs_rev, l)
              :: (Segment.of_sides [Segment.Right] :: segs_rev, r)
              :: tail)
  in
  aux [] [([], node)]

let every_leaves context node : Segment.t list list =
  let rec aux store = function
    | [] -> store
    | (pathes_rev, node) :: tail ->
       all_children context node
       |> List.map (function
              |  (seg, `File) -> List.rev (seg :: pathes_rev) :: store
              | (seg, `Directory child) when Segment.is_empty seg ->
                 aux store ((pathes_rev, child) :: tail)
              | (seg, `Directory child) ->
                 aux store ((seg :: pathes_rev, child) :: tail))
       |> List.concat
  in
  aux [] [([], node)]

let xassert b =
  let open Printexc in
  if not b then begin
    prerr_endline ("*****************************************************\n" ^ raw_backtrace_to_string (get_callstack 10));
    assert false
  end

