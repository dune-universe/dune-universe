(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Arthur Breitman                                        *)
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
open Result

type hashed =
  | Hashed of Hash.t
  | Not_Hashed
  (** Type used to prove that if a node is hashed then so are its children.
      The type also provides the hash as a witness.*)

type indexed =
  | Indexed of Index.t
  | Not_Indexed
  (** This rule expresses the following invariant : if a node is indexed, then
      its children are necessarily indexed. Less trivially, if an internal node is not
      indexed then at least one of its children is not yet indexed. The reason
      is that we never construct new nodes that just point to only existing nodes. 
      This property guarantees that when we write internal nodes on
      disk, at least one of the child can be written adjacent to its parent. *)

type extender_witness =
  | Maybe_Extender
  | Not_Extender  
  | Is_Extender   

type node =
  | Disk of Index.t * extender_witness
  (* Represents a node stored on the disk at a given index, the node hasn't
     been loaded yet. Although it's considered hashed for simplicity's sake,
     reading the hash requires a disk access and is expensive. *)

  | View of view
  (* A view node is the in-memory structure manipulated by programs in order
     to compute edits to the context. New view nodes can be commited to disk
     once the computations are done. *)

and view =
  | Internal of node * node
               * indexed
               * hashed

  (* An internal node , left and right children and an internal path segment
     to represent part of the path followed by the key in the tree. *)

  | Bud of node option
          * indexed
          * hashed
  (* Buds represent the end of a segment and the beginning of a new tree. They
     are used whenever there is a natural hierarchical separation in the key
     or, in general, when one wants to be able to grab sub-trees. For instance
     the big_map storage of a contract in Tezos would start from a bud. *)

  | Leaf of Value.t
          * indexed
          * hashed
  (* Leaf of a tree, the end of a path, contains or points to a value.
     The current implementation is a bit hackish and leaves are written
     on *two* cells, not one. This is important to keep in mind when
     committing the tree to disk.
  *)

  | Extender of Segment.t
                * node
                * indexed
                * hashed
  (* Extender node, contains a path to the next node. Represents implicitely
     a collection of internal nodes where one child is Null. *)

(* A trail represents the content of the memory stack when recursively exploring a tree.
   Constructing these trails from closure would be easier, but it would make it harder
   to port the code to C. The type parameters of the trail keep track of the type of each
   element on the "stack" using a product type. *)

type t = node

type Error.t += Node_invariant of string
let () = Error.register_printer (function
    | Node_invariant s -> Some ("Node invariant: " ^ s)
    | _ -> None)

let error_node_invariant s = Error (Node_invariant s)

let view_shape_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (None, _, _) -> Ok ()
  | Bud (Some (Disk _), _, _) -> error_node_invariant "Bud: cannot have Disk" (* or, we must load the Disk and check *)
  | Bud (Some (View (Bud _)), _, _) -> error_node_invariant "Bud: cannot have Bud"
  | Bud (Some (View (Leaf _)), _, _) -> error_node_invariant "Bud: cannot have Leaf"
  | Bud (Some (View (Internal _)), _, _) -> Ok ()
  | Bud (Some (View (Extender _)), _, _) -> Ok ()
  | Extender (seg, _, _, _) when Segment.is_empty seg -> error_node_invariant "Extender: cannot have empty segment"
  | Extender (_, Disk (_, Not_Extender), _, _) -> Ok ()
  | Extender (_, Disk (_, Is_Extender), _, _) -> error_node_invariant "Extender: cannot have Disk with Is_Extender"
  | Extender (_, Disk (_, Maybe_Extender), _, _) -> error_node_invariant "Extender: cannot have Disk with Maybe_Extender"
  | Extender (_, View (Extender _), _, _) -> error_node_invariant "Extender: cannot have Extender"
  | Extender (_, View _, _, _) -> Ok ()
  | Leaf _ -> Ok ()
  | Internal _ -> Ok ()

let indexed = function
  | Disk _ -> true
  | View (Bud (_, Indexed _, _)) -> true
  | View (Leaf (_, Indexed _, _)) -> true
  | View (Internal (_, _, Indexed _, _)) -> true
  | View (Extender (_, _, Indexed _, _)) -> true
  | View (Bud _ | Leaf _ | Internal _ | Extender _) -> false

let index_of_view = function
  | Bud (_, Indexed i, _) -> Some i
  | Leaf (_, Indexed i, _) -> Some i
  | Internal (_, _, Indexed i, _) -> Some i
  | Extender (_, _, Indexed i, _) -> Some i
  | Bud _ | Leaf _ | Internal _ | Extender _ -> None

let index = function
  | Disk (i,_) -> Some i
  | View v -> index_of_view v

let view_indexed_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (None, Indexed _, _) -> Ok ()
  | Bud (Some n, Indexed _, _) when indexed n -> Ok ()
  | Bud (_, Not_Indexed, _) -> Ok ()
  | Leaf (_, Indexed _, _) -> Ok ()
  | Leaf (_, Not_Indexed, _) -> Ok ()
  | Internal (l, r, Indexed _i, _) ->
      begin match index l, index r with
        | None, _ -> error_node_invariant "Internal: invalid Indexed"
        | _, None -> error_node_invariant "Internal: invalid Indeced"
(* We abandoned this strict invariant on internals.
        | Some li, Some ri -> 
            if Index.(i - li = one || i - ri = one) then Ok ()
            else error_node_invariant "Internal: invalid indices"
*)
        | _ -> Ok () (* we now use fat internals *)
      end
  | Internal (_l, _r, Not_Indexed, _) -> Ok ()
  | Extender (_, n, Indexed _, _) when indexed n -> Ok ()
  | Extender (_, _, Not_Indexed, _) -> Ok ()
  | Bud (_, Indexed _, _)  
  | Extender (_, _, Indexed _, _)  -> error_node_invariant "Invalid Indexed"

let hashed = function
  | Disk _ -> true
  | View (Bud (_, _, Hashed _)) -> true
  | View (Bud (_, _, Not_Hashed)) -> false
  | View (Leaf (_, _, Hashed _)) -> true
  | View (Leaf (_, _, Not_Hashed)) -> false
  | View (Internal (_, _, _, Hashed _)) -> true
  | View (Internal (_, _, _, Not_Hashed)) -> false
  | View (Extender (_, _, _, Hashed _)) -> true
  | View (Extender (_, _, _, Not_Hashed)) -> false

let hash_of_view = function
  | (Bud (_, _, Hashed h)) -> Some h
  | (Bud (_, _, Not_Hashed)) -> None
  | (Leaf (_, _, Hashed h)) -> Some h
  | (Leaf (_, _, Not_Hashed)) -> None
  | (Internal (_, _, _, Hashed h)) -> Some h
  | (Internal (_, _, _, Not_Hashed)) -> None
  | (Extender (_, _, _, Hashed h)) -> Some h
  | (Extender (_, _, _, Not_Hashed)) -> None

let view_hashed_invariant : view -> (unit, Error.t) Result.t = function
  | Leaf _ -> Ok ()
  | Bud (None, _, _) -> Ok ()
  | Bud (_, _, Not_Hashed) -> Ok ()
  | Bud (Some n, _, Hashed _) when hashed n -> Ok ()
  | Internal (l, r, _, Hashed _) when hashed l && hashed r -> Ok ()
  | Internal (_, _, _, Not_Hashed) -> Ok ()
  | Extender (_, n, _, Hashed _) when hashed n -> Ok ()
  | Extender (_, _, _, Not_Hashed) -> Ok ()
  | _ -> error_node_invariant "Invalid Hashed"

let view_index_and_hash_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (_, Indexed _, Not_Hashed)
  | Leaf (_, Indexed _, Not_Hashed)
  | Internal (_, _, Indexed _, Not_Hashed)
  | Extender (_, _, Indexed _, Not_Hashed) -> error_node_invariant "View: Indexed with Not_Hashed"
  | _ -> Ok ()

let view_invariant : view -> (unit, Error.t) Result.t = fun v ->
  view_shape_invariant v >>= fun () ->
  view_indexed_invariant v >>= fun () ->
  view_hashed_invariant v >>= fun () ->
  view_index_and_hash_invariant v

let check_view v = 
  match view_invariant v with
  | Ok _ -> v
  | Error e -> failwith (Error.show e)

let _Internal (n1, n2, ir, hit) =
  check_view @@ Internal (n1, n2, ir, hit)

let _Bud (nopt, ir, hit) =
  check_view @@ Bud (nopt, ir, hit)

let _Leaf (v, ir, hit) =
  check_view @@ Leaf (v, ir, hit)

let _Extender (p, n, ir, hit) =
  check_view @@ Extender (p, n, ir, hit)

let new_leaf v = View (_Leaf (v, Not_Indexed, Not_Hashed))

let new_extender : Segment.segment -> node -> node = fun segment node ->
  if Segment.is_empty segment then node (* XXX We should simply reject it *)
  else 
    match node with
    | View (Extender (seg, n, _, _)) ->
        View (_Extender (Segment.append segment seg, n, Not_Indexed, Not_Hashed))
    | _ ->
        (* XXXX If the subnode is Disk, we have to make sure merging *)
        View (_Extender (segment, node, Not_Indexed, Not_Hashed))

let new_bud no = View (_Bud (no, Not_Indexed, Not_Hashed))

let new_internal n1 n2 = View (_Internal (n1, n2, Not_Indexed, Not_Hashed))

let load_node_ref = ref (fun _ _ _ -> assert false)

let load_node context index ewit = !load_node_ref context index ewit

let may_forget = function
  | Disk _ as n -> Some n
  | View (Internal (_, _, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Bud (_, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Leaf (_, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Extender (_, _, Indexed i, _)) -> Some (Disk (i, Is_Extender))
  | _ -> None

let view c = function
  | Disk (i, wit) -> load_node c i wit
  | View v -> v

let rec pp ppf = function
  | Disk (i, ew) ->
      Format.fprintf ppf "Disk (%ld, %s)" (Index.to_int32 i)
        (match ew with
         | Maybe_Extender -> "Maybe_Ex"
         | Not_Extender -> "Not_Ex"
         | Is_Extender -> "Is_Ex")
  | View v ->
      let f = function
        | Internal (n1, n2, _i, _h) ->
            Format.fprintf ppf "@[<v2>Internal@ L %a@ R %a@]"
              pp n1
              pp n2
        | Bud (None, _, _) ->
            Format.fprintf ppf "Bud None"
        | Bud (Some n, _, _) ->
            Format.fprintf ppf "@[<v2>Bud@ %a@]"
              pp n
        | Leaf (v, _, _) ->
            Format.fprintf ppf "Leaf %s"
              (let s = Value.to_hex_string v in
               if String.length s > 16 then String.sub s 0 16 else s)
        | Extender (seg, n, _, _) ->
            Format.fprintf ppf "@[<v2>Extender %s@ %a@]"
              (Segment.to_string seg)
              pp n
      in
      f v
