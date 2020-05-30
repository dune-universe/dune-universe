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
open Node
open Result
open Cursor

(** Multi Bud level interface *)
let deep ~go_up ~create_subtrees cur segs f =
  let rec ups cur = function
    | [] -> Ok cur
    | _seg::segs -> parent cur >>= fun cur -> ups cur segs
  in
  let rec aux hist cur = function
    | [] -> assert false
    | [seg] ->
        f cur seg >>= fun (cur, res) ->
        if go_up then
          ups cur hist >>= fun cur ->
          Ok (cur, res)
        else
          Ok (cur, res)
    | seg::segs ->
        (if create_subtrees then subtree_or_create else subtree) cur seg >>= fun cur ->
        aux (seg::hist) cur segs
  in
  aux [] cur segs

let get cur segs =
  deep ~go_up:true ~create_subtrees:false cur segs get

let get_value cur segs =
  deep ~go_up:true ~create_subtrees:false cur segs get_value

let upsert cur segs v =
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      upsert cur seg v >>| fun cur -> cur, ()) >>| fst

let insert cur segs v =
  deep ~go_up:true ~create_subtrees:true cur segs (fun cur seg ->
      insert cur seg v >>| fun cur -> cur, ()) >>| fst

let update cur segs v =
  deep ~go_up:true ~create_subtrees:false cur segs (fun cur seg ->
      Cursor.get cur seg >>= fun _ -> update cur seg v >>| fun cur -> cur, ()) >>| fst

let delete cur segs =
  deep ~go_up:true ~create_subtrees:false cur segs (fun cur seg ->
      delete cur seg >>| fun cur -> cur, ()) >>| fst

let delete_and_clean_empty cur segs =
  deep ~go_up:false ~create_subtrees:false cur segs (fun cur seg ->
      match Cursor.delete cur seg with
      | Ok cur -> Ok (cur, ())
      | Error _ -> Ok (cur, ())) >>| fst
  >>= Cursor.remove_empty_bud

let create_subtree ~create_subtrees cur segs =
  deep ~go_up:true ~create_subtrees cur segs (fun cur seg ->
      create_subtree cur seg >>| fun cur -> (cur, ())) >>| fst

let subtree cur segs =
  deep ~go_up:false ~create_subtrees:false cur segs (fun cur seg ->
      subtree cur seg >>| fun cur -> (cur, ())) >>| fst

let subtree_or_create ~create_subtrees cur segs =
  deep ~go_up:false ~create_subtrees cur segs (fun cur seg ->
      subtree_or_create cur seg >>| fun cur -> (cur, ())) >>| fst

(* bud copy by link *)
let copy ~create_subtrees cur segs1 segs2 =
  let rec is_prefix segs1 segs2 = match segs1, segs2 with
    | [], _ -> true
    | seg1::segs1, seg2::segs2 when seg1 = seg2 -> is_prefix segs1 segs2
    | _ -> false
  in
  if is_prefix segs1 segs2 then Error (Cursor.Write "copy: it creates a loop!")
  else
    deep ~go_up:false ~create_subtrees:false cur segs1
      (fun cur seg -> access_gen cur seg >>= function
         | Reached (cur, (Bud _ as bud)) -> Ok (cur, bud)
         | res -> error_access res) >>= fun (_, bud) ->
    (* bud is copied. Therefore this will not break the internal node's
       invariant.  (If we share the bud for further optmization,
       then it breaks the invariant.)
    *)
    deep ~go_up:true ~create_subtrees cur segs2
      (fun cur seg ->
         alter cur seg (function
             | None -> Ok (View bud)
             | Some _ -> Error (Cursor.Write "a node already presents for this segment")) >>= fun cur ->
         Ok (cur, ())) >>| fst

let link n cur segs =
  deep ~go_up:true ~create_subtrees:true
    cur
    segs
    (fun c seg ->
       (* XXX this alter has once inserted Disk (Maybe_Extender)
          under an extender and crashed program.

          We here workaround this problem to force to view [n]
       *)
       let Cursor (_, _, context) = c in
       (* [n] must be under [context] *)
       let v = Node.view context n in
       let n = View v in
       alter c seg (function
           | Some _ -> assert false
           | None -> Ok n) >>| fun c -> (c, ()))
  >>| fst
