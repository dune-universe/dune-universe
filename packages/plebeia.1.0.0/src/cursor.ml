(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Arthur Breitman <arthur.breitman+nospam@tezos.com>     *)
(* Copyright (c) 2019 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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
(** Cursor (zipper) based tree operations *)

open Utils.Open
open Result
open Node

(* Trail and cursor *)

type modified =
  | Modified
  | Unmodified of indexed * hashed

type trail =
  | Top
  | Left of (* we took the left branch of an internal node *)
      trail
      * node
      * modified

  | Right of (* we took the right branch of an internal node *)
      node
      * trail
      * modified

  | Budded of
      trail
      * modified

  | Extended of
      trail
      * Segment.t
      * modified
  (* not the use of the "extender" and "not extender" type to enforce
     that two extenders cannot follow each other *)

type Error.t += Cursor_invariant of string
let () = Error.register_printer @@ function
  | Cursor_invariant s -> Some ("Cursor: " ^ s)
  | _ -> None

let error_cursor_invariant s = Error (Cursor_invariant s)

let trail_shape_invariant = function
  | Extended (Extended _, _, _) -> error_cursor_invariant "Extended: cannot have Extended"
  | Extended (_, seg, _) when Segment.is_empty seg -> error_cursor_invariant "Extended: invalid empty segment"
  | _ -> Ok ()

let trail_modified_invariant = function
  | Top -> Ok ()
  | Left (_, n, Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> error_cursor_invariant "Left: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> error_cursor_invariant "Left: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Left (_, _, Modified) -> Ok ()
  | Right (n, _, Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> error_cursor_invariant "Right: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> error_cursor_invariant "Right: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Right (_, _, Modified) -> Ok ()
  | Budded (_, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ | Not_Indexed -> Ok ()
      end
  | Budded (_, Modified) -> Ok ()
  | Extended (_, _, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ | Not_Indexed -> Ok ()
      end
  | Extended (_, _, Modified) -> Ok ()

let trail_index_and_hash_invariant = function
  | Top -> Ok ()
  | Left (_, _, Unmodified (Indexed _, Not_Hashed))
  | Right (_, _, Unmodified (Indexed _, Not_Hashed))
  | Budded (_, Unmodified (Indexed _, Not_Hashed))
  | Extended (_, _, Unmodified (Indexed _, Not_Hashed)) -> error_cursor_invariant "Trail: Indexed with Not_Hashed"
  | _ -> Ok ()

let trail_invariant t =
  trail_shape_invariant t >>= fun () ->
  trail_modified_invariant t >>= fun () ->
  trail_index_and_hash_invariant t

let check_trail t =
  match trail_invariant t with
  | Ok _ -> t
  | Error s -> Error.raise s

let _Top = Top
let _Left (t, n, mr)     = check_trail @@ Left (t, n, mr)
let _Right (n, t, mr)    = check_trail @@ Right (n, t, mr)
let _Budded (t, mr)      = check_trail @@ Budded (t, mr)
let _Extended (t, s, mr) = check_trail @@ Extended (t, s, mr)

type cursor =
    Cursor of trail
              * node
              * Context.t
(* The cursor, also known as a zipper combines the information contained in a
   trail and a subtree to represent an edit point within a tree. This is a
   functional data structure that represents the program point in a function
   that modifies a tree. We use an existential type that keeps the .mli sane
   and enforces the most important: that the hole tags match between the trail
   and the Node *)

type t = cursor

let context (Cursor (_, _, context)) = context

let cursor_invariant (Cursor (trail, n, c)) =
  match trail with
  | Top ->
      begin match view c n with
        | Bud _ -> Ok ()
        | v ->
            Log.fatal "@[<v2>Cursor: Top has no Bud:@ %a@]" Node.pp (View v);
            Error "Cursor: Top has no Bud"
      end
  | Left (_, n', Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n && indexed n' -> Ok ()
        | Indexed _ -> Error "Cursor: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Cursor: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Left (_, _, Modified) -> Ok ()
  | Right (n', _, Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n && indexed n' -> Ok ()
        | Indexed _ -> Error "Cursor: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Cursor: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Right (_, _, Modified) -> Ok ()
  | Budded (_, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Budded: invalid Indexed"
        | Not_Indexed -> Ok ()
      end
  | Budded (_, Modified) -> Ok ()
  | Extended (_, _, Unmodified (ir, hit)) ->
      begin match ir with
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Extended: invalid Indexed"
        | Not_Indexed -> Ok ()
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Extended: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Extended (_, _, Modified) -> Ok ()

let check_cursor c =
  match cursor_invariant c with
  | Ok _ -> c
  | Error s -> failwith s

let _Cursor (t, n, c) =
  check_cursor @@ Cursor (t, n, c)

let segs_of_trail trail =
  let rec aux (xs, xss) = function
    | Top ->
        (* The previous trail must be Budded, therefore xs must be empty *)
        assert (Segment.is_empty xs); xss
    | Budded (tr, _) -> aux (Segment.empty, xs::xss) tr
    | Left (tr, _, _) -> aux (Segment.(cons Left xs), xss) tr
    | Right (_, tr, _) -> aux (Segment.(cons Right xs), xss) tr
    | Extended (tr, seg, _) -> aux (Segment.append seg xs, xss) tr
  in
  aux (Segment.empty, []) trail

let segs_of_cursor (Cursor (trail, _, _)) = segs_of_trail trail

let local_seg_of_trail trail =
  let rec aux xs = function
    | Top -> xs
    | Budded (_, _) -> xs
    | Left (tr, _, _) -> aux (Segment.cons Left xs) tr
    | Right (_, tr, _) -> aux (Segment.cons Right xs) tr
    | Extended (tr, seg, _) -> aux (Segment.append seg xs) tr
  in
  aux Segment.empty trail

let local_seg_of_cursor (Cursor (trail, _, _)) = local_seg_of_trail trail

let dot_of_cursor_ref = ref (fun _ -> assert false)

let attach trail node context =
  (* Attaches a node to a trail even if the indexing type and hashing type is
     incompatible with the trail by tagging the modification. Extender types
     still have to match. *)
  match trail with
  | Top -> _Cursor (_Top, node, context)
  | Left (prev_trail, right, _) ->
      _Cursor (_Left (prev_trail, right, Modified), node, context)
  | Right (left, prev_trail, _) ->
      _Cursor (_Right (left, prev_trail, Modified), node, context)
  | Budded (prev_trail, _) ->
      _Cursor (_Budded (prev_trail, Modified), node, context)
  | Extended (prev_trail, segment, _) ->
      _Cursor (_Extended (prev_trail, segment, Modified), node, context)

(* Follow the segment from the cursor. If the segment terminates
  or diverges in the middle of an extender, it returns the common prefix
  information.
*)
type access_result =
  | Empty_bud (* The bud is empty *)
  | Collide of cursor * view (* The segment was blocked by an existing leaf or bud *)
  | Middle_of_extender of cursor * Segment.t * Segment.t * Segment.t (* The segment ends or diverges at the middle of an Extender with the common prefix, the remaining extender, and the rest of segment *)
  | Reached of cursor * view (* just reached to a node *)

type Error.t +=
  | Access of access_result
  | Move of string
  (*  | Cursor_other of string *)

let () = Error.register_printer (function
    | Access a ->
        Some (match a with
            | Empty_bud -> "Nothing beneath this bud"
            | Collide _ -> "Reached to a leaf or bud before finishing"
            | Middle_of_extender (_, _, _, seg) when Segment.is_empty seg ->
                "Finished at the middle of an Extender"
            | Middle_of_extender (_, _, _, _) ->
                "Diverged in the middle of an Extender"
            | Reached (_, Bud _) -> "Reached to a Bud"
            | Reached (_, Leaf _) -> "Reached to a Leaf"

            | Reached (_, Internal _) -> "Reached to an Internal"
            | Reached (_, Extender _) -> "Reached to an Extender")
    | Move s -> Some s
    (*    | Cursor_other s -> Some s *)
    | _ -> None)

let error_access a = Error (Access a)

let view_cursor (Cursor (trail, n, context)) =
  let v = view context n in
  (_Cursor (trail, View v, context), v)

let index (Cursor (_, n, _)) = Node.index n

let go_below_bud (Cursor (trail, n, context)) =
  (* This function expects a cursor positionned on a bud and moves it one step below. *)
  match view context n with
  | Bud (None,  _, _) -> Ok None
  | Bud (Some below, indexed, hashed) ->
      Ok (Some (_Cursor (
          _Budded (trail, Unmodified (indexed, hashed)), below,  context)))
  | _ -> Error (Move "Attempted to navigate below a bud, but got a different kind of node.")

let go_side side (Cursor (trail, n, context)) =
  (* Move the cursor down left or down right in the tree, assuming we are on an internal node. *)
  match view context n with
  | Internal (l, r, indexed, hashed) ->
      Ok (match side with
          | Segment.Right ->
              _Cursor (_Right (l, trail,
                             Unmodified (indexed, hashed)),
                       r, context)
          | Segment.Left ->
              _Cursor (_Left (trail, r,
                            Unmodified (indexed, hashed)),
                       l, context))
  | _ -> Error (Move "Attempted to navigate right or left of a non internal node")

let go_down_extender (Cursor (trail, n, context)) =
  (* Move the cursor down the extender it points to. *)
  match view context n with
  | Extender (segment, below, indexed, hashed) ->
      Ok (_Cursor (_Extended (trail, segment,
                            Unmodified (indexed, hashed)),
                   below, context))
  | _ -> Error (Move "Attempted to go down an extender but did not find an extender")

(* Go up 1 level of tree.
   Note that this can be more than 1 levels in segments,
   because of the extenders
*)
let go_up (Cursor (trail, node, context))  = match trail with
  | Top -> Error (Move "cannot go above top")
  | Left (prev_trail, right,
          Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Internal (node, right, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Right (left, prev_trail,
           Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Internal (left, node, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Budded (prev_trail,
            Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Bud (Some node, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Extended (prev_trail, segment,
              Unmodified (indexed, hashed)) ->
    let new_node =
      View (_Extender (segment, node, indexed, hashed))
    in Ok (_Cursor (prev_trail, new_node, context))

  (* Modified cases. *)

  | Left (prev_trail, right, Modified) ->
      let internal = new_internal node right in
      Ok (attach prev_trail internal context)

  | Right (left, prev_trail, Modified) ->
      let internal = new_internal left node in
      Ok (attach prev_trail internal context)

  | Budded (prev_trail, Modified) ->
      let bud = new_bud @@ Some node in
      Ok (attach prev_trail bud context)

  | Extended (prev_trail, segment, Modified) ->
      let extender = new_extender segment node in
      Ok (attach prev_trail extender context)

let rec go_top (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> Ok c
  | _ -> go_up c >>= go_top

let rec go_up_to_a_bud c =
  let c, v = view_cursor c in
  match v with
  | Bud _ -> Ok c (* already at a bud *)
  | _ -> go_up c >>= go_up_to_a_bud
(* XXX We can check trail instead of view *)

let rec go_up_to_bud c =
  go_up c >>= fun c ->
  let c, v = view_cursor c in
  match v with
  | Bud _ -> Ok c (* reached to a bud *)
  | _ -> go_up_to_bud c

let parent c =
  let c, v = view_cursor c in
  match v with
  | Bud _ -> go_up_to_bud c
  | _ -> Error (Move "parent: cursor must be at a bud")

let unify_extenders prev_trail node context = match node with
  | Disk (_, Is_Extender) -> Error (Move "unify_exenders: Disk is not allowed")
  | View (Extender (seg, n, _, _)) ->
      begin match prev_trail with
        | Extended (prev_trail', seg', _mr) ->
            Ok (attach prev_trail' (new_extender (Segment.append seg' seg) n) context)
        | _ -> Ok (attach prev_trail node context)
      end
  | _ -> Ok (attach prev_trail node context)

let rec remove_up trail context = match trail with
  | Top -> Error (Move "cannot remove top") (* XXX *)
  | Budded (prev_trail, _) ->
      Ok (attach prev_trail (new_bud None) context)
  | Extended (prev_trail, _, _) -> remove_up prev_trail context
  (* for Left and Right, we may need to squash Extenders in prev_trail *)
  | Left (prev_trail, right, _) ->
      (*
               /
              /\
         --> *  r

         We must load r because r can be an extender!
      *)
      let right = View (view context right) in
      let n = new_extender Segment.(of_sides [Right]) right in
      unify_extenders prev_trail n context
  | Right (left, prev_trail, _) ->
      (*
               /
              /\
             l  * <--

         We must load l because l can be an extender!
      *)
      let left = View (view context left) in
      let n = new_extender Segment.(of_sides [Left]) left in
      unify_extenders prev_trail n context

(* Let [c] is a cursor which points an Extender, whose segment is [common_prefix @ remaining_extender].
   [diverge c (common_prefix, remaining_extender, remaining_segment)] diverges a segment of [c] in the middle
   and create a path to [common_prefix @ remaining_segnet].
   It returns the newly created trail.
*)
let diverge (Cursor (trail, extender, _context)) (common_prefix, remaining_extender, remaining_segment) =
  match extender with
  | View (Extender (_seg, n, _ir, _hit)) -> (* _seg = common_prefix @ remaining_extender *)
      begin match Segment.cut remaining_segment, Segment.cut remaining_extender with
        | None, _ -> error_cursor_invariant "diverge: remaining_segment is empty"
        | _, None -> error_cursor_invariant "diverge: remaining_extender is empty"
        | Some (side, seg), Some (side', seg') ->
            (* go down along common_prefix *)
            assert (side <> side');
            let trail =
              if Segment.is_empty common_prefix then trail
              else _Extended (trail, common_prefix, Modified)
            in
            let n' = new_extender seg' n in
            match side with
            | Segment.Left ->
                if Segment.is_empty seg then
                  Ok (_Left (trail, n', Modified))
                else
                  Ok (_Extended (_Left (trail, n', Modified), seg, Modified))
            | Segment.Right ->
                if Segment.is_empty seg then
                  Ok (_Right (n', trail, Modified))
                else
                  Ok (_Extended (_Right (n', trail, Modified), seg, Modified))
      end
  | _ -> error_cursor_invariant "diverge: not an Extender"

let access_gen' cur segment =
  (* returns the cursor found by following the segment from the given cursor *)
  let rec aux (Cursor (trail, n, context)) segment =
    let v = view context n in
    let cur = _Cursor (trail, View v, context) in
    match Segment.cut segment with
    | None -> Ok (Reached (cur, v))
    | Some (dir, segment_rest) ->
        match v with
        | Leaf _ | Bud _ ->  Ok (Collide (cur, v))
        | Internal (l, r, indexed, hashed) -> begin
            match dir with
            | Left ->
                let new_trail = _Left (trail, r, Unmodified (indexed, hashed)) in
                aux (_Cursor (new_trail, l, context)) segment_rest
            | Right ->
                let new_trail = _Right (l, trail, Unmodified (indexed, hashed)) in
                aux (_Cursor (new_trail, r, context)) segment_rest
          end
        | Extender (extender, node_below, indexed, hashed) ->
            let (shared, remaining_extender, remaining_segment) =
              Segment.common_prefix extender segment in
            if Segment.is_empty remaining_extender then
              let new_trail =
                _Extended (trail, extender, Unmodified (indexed, hashed)) in
              aux (_Cursor (new_trail, node_below, context)) remaining_segment
            else
              Ok (Middle_of_extender (cur, shared, remaining_extender, remaining_segment))
  in
  aux cur segment

(* [cur] must point to a bud.
   If [segment = []], returns a node attached to the bud.
*)
let access_gen cur segment =
  go_below_bud cur >>= function
  | None -> Ok Empty_bud
  | Some cur -> access_gen' cur segment

let subtree cur seg =
   access_gen cur seg >>= function
   | Reached (cur, Bud _) -> Ok cur
   | res -> error_access res

let get cur seg =
  access_gen cur seg >>= function
  | Reached (c, (Bud _ as v)) -> go_up_to_bud c >>= fun c -> Ok (c, `Bud v)
  | Reached (c, (Leaf _ as v)) -> go_up_to_bud c >>= fun c -> Ok (c, `Leaf v)
  | res -> error_access res

let get_value cur seg =
  access_gen cur seg >>= function
  | Reached (c, Leaf (v, _, _)) ->
      go_up_to_bud c >>= fun c -> Ok (c, v)
  | Reached _ as res -> error_access res (* XXX throwing away the updated cur... *)
  | res -> error_access res

let empty context =
  (* A bud with nothing underneath, i.e. an empty tree or an empty sub-tree. *)
  _Cursor (_Top, new_bud None, context)

let delete cur seg =
  access_gen cur seg >>= function
  | Reached (Cursor (trail, _, context), (Bud _ | Leaf _)) ->
      remove_up trail context
      >>= go_up_to_a_bud
  | res -> error_access res

let alter (Cursor (trail, _, context) as cur) segment alteration =
  access_gen cur segment >>= function
  | Empty_bud ->
      alteration None >>= fun n ->
      let n = new_extender segment n in
      let n = new_bud (Some n) in (* This replaces the current empty bud *)
      Ok (attach trail n context)
  | (Middle_of_extender (_, _, _, seg) as res) when Segment.is_empty seg -> error_access res
  | (Reached (c, _) | Middle_of_extender (c, _, _, _) as res) ->
      (* XXX cleanup required *)
      let segsopt = match res with
        | Reached _ -> None
        | Middle_of_extender (_c, shared, rest_extender, rest_segment) ->
            Some (shared, rest_extender, rest_segment)
        | _ -> assert false
      in
      begin match segsopt with
        | None ->
            (* Should we view the node? *)
            let Cursor (trail, n, context) = c in
            let v = view context n in
            Ok (trail, Some v)
        | Some segs -> diverge c segs >>| fun trail -> (trail, None)
      end >>= fun (trail, vo) ->
      alteration vo >>= fun n ->
      (* Skip the alteration if identical *)
      let no_mod = match vo, n with
        | Some v, View v' when v == v' -> true
        | Some (Leaf (v, i, h)), View (Leaf (v', i', h')) when v = v' ->
            begin match (i, i'), (h, h') with
              | (Indexed _, _ | Not_Indexed, Not_Indexed),
                (Hashed _, _ | Not_Hashed, Not_Hashed) -> true
              | _ -> false
            end
        | _ -> false
      in
      let c =
        if no_mod then c else attach trail n context
      in
      (* go_up is required since c may point to a new bud *)
      go_up c >>= go_up_to_a_bud
  | res -> error_access res

let update cur segment value =
  access_gen cur segment >>= function
  | Reached (Cursor (trail, _, context), Leaf _) ->
      go_up_to_a_bud (attach trail (View (_Leaf (value, Not_Indexed, Not_Hashed))) context)
  | res -> error_access res

type Error.t +=
  | Write of string
let () = Error.register_printer @@ function
  | Write s -> Some ("Write: " ^ s)
  | _ -> None

let upsert cur segment value =
  alter cur segment (fun x ->
     let y = Ok (new_leaf value) in
     match x with
     | None -> y
     | Some (Leaf _) -> y
     | Some _ -> Error (Write "a non Leaf node already presents for this path"))

let insert cur segment value =
  alter cur segment (function
      | None -> Ok (View (_Leaf (value, Not_Indexed, Not_Hashed)))
      | Some _ -> Error (Write "a node already presents for this path"))

let create_subtree cur segment =
  alter cur segment (function
      | None -> Ok (new_bud None)
      | Some _ -> Error (Write "a node already presents for this path"))

let subtree_or_create cur segment =
  (* XXX inefficient.  create_subtree should have an option not to go back to the original position *)
  let cur =
    match create_subtree cur segment with
    | Ok cur -> cur
    | Error _ -> cur
  in
  subtree cur segment

(* XXX bug.  No point using the Cursor. *)
let traverse acc cs f = match cs with
  | [] -> acc, []
  | c::cs ->
      let c, v = view_cursor c in
      match f acc c with
      | `Exit, acc -> acc, []
      | `Up, acc -> acc, cs
      | `Continue, acc ->
          match v with
          | Leaf _ | Bud (None, _, _) -> acc, cs
          | Bud (Some _, _, _) ->
              acc, from_Some (from_Ok (go_below_bud c)) :: cs
          | Internal (_, _, _, _) ->
              let c1 = from_Ok @@ go_side Left c in
              let c2 = from_Ok @@ go_side Right c in
              acc, c1 :: c2 :: cs
          | Extender (_, _, _, _) ->
              acc, from_Ok (go_down_extender c) ::cs

let fold ~init c f =
  let rec aux acc cs = match traverse acc cs f with
    | acc, [] -> acc
    | acc, cs -> aux acc cs
  in
  aux init [c]

let stat (Cursor (_,_,{ stat ; _ })) = stat

let view = view_cursor

(* XXX can be rewritten in a cleaner way? *)
let remove_empty_bud c =
  match view c with
  | Cursor (Top, _, _), _ -> Ok c
  | c, Bud (None, _, _) ->
      let rec find c =
        match go_up c with
        | Error _ -> c
        | Ok (Cursor (Top, _, _)) -> c
        | Ok c' ->
            let c', v = view c' in
            match v with
            | Leaf _ -> assert false
            | Bud (None, _, _) -> assert false
            | Bud (Some _, _, _) | Extender _ -> find c'
            | Internal _ -> c
      in
      let Cursor (trail, _, context) = find c in
      remove_up trail context
  | _ -> Ok c

let may_forget c =
  let Cursor (trail, n, context) = c in
  match Node.may_forget n with
  | None -> None
  | Some n -> Some (_Cursor (trail, n, context))

(*
let clean_bud c =
  let (Cursor (trail, _, context) as c), v = view c in
  match v with
  | Bud (None, _, _) -> Ok c
  | Bud (Some _, _, _) -> Ok (_Cursor (trail, new_bud None, context))
  | _ -> Error (Cursor_other "clean_bud: it is not a bud")
*)
