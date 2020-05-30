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
open Result

type hash = Node_hash.t

type elem =
  | ProofInternal of hash
  | ProofExtender of Segment.t

type step_proof = elem list

type t = {
    proof_stream : step_proof list;
    data : Value.t
  }

module E = Data_encoding

let step_proof_encoding =
  let elem_encoding =
    E.union [
      (E.case ~title:"ProofInternal" E.Json_only Node_hash.encoding
         (function ProofInternal h -> Some h | _ -> None)
         (fun h -> ProofInternal h));
      (E.case ~title:"ProofExtender" E.Json_only Segment.encoding
         (function ProofExtender seg -> Some seg | _ -> None)
         (fun seg -> ProofExtender seg));
      ]
  in
  E.list elem_encoding

let encoding =
  E.obj2
    (E.req "proof_stream" (E.list step_proof_encoding))
    (E.req "data" Value.encoding)
  |> E.conv (fun t -> (t.proof_stream, t.data))
  (fun (proof_stream, data) -> {proof_stream; data})

let get_data t = t.data

type Error.t += NonExistentPath of Segment.t list
let () = Error.register_printer @@ function
  | NonExistentPath path ->
    List.map Segment.to_string path
    |> String.concat " / "
    |> (^) "NonExistentPath: "
    |> fun s -> Some s
  | _ -> None

let rm_prefix prefix seg =
  let (_, prefix', seg') = Segment.common_prefix prefix seg in
  if Segment.is_empty prefix' then Ok seg'
  else Error `NonExistentPath

let generate_1step_from_node context seg node =
  let rec iter_view store seg view =
    match (Segment.cut seg, view) with
    | (None, Node.Leaf _) | (None, Node.Bud _) ->
       Ok (List.rev store, view)
    | (None, Node.Internal _) | (None, Node.Extender _) ->
       Error `NonExistentPath
    | (Some _, Node.Leaf _) | (Some _, Node.Bud _) ->
       Error `NonExistentPath
    | (Some (Segment.Left, seg'), Node.Internal (l, r, _, _)) ->
       let (_, right_h) = Node_hash.compute context r in
       iter_node (ProofInternal right_h :: store) seg' l
    | (Some (Segment.Right, seg'), Node.Internal (l, r, _, _)) ->
       let (_, left_h) = Node_hash.compute context l in
       iter_node (ProofInternal left_h :: store) seg' r
    | (Some (_, _), Node.Extender (prefix, node, _, _)) ->
       rm_prefix prefix seg >>= fun seg' ->
       iter_node (ProofExtender prefix :: store) seg' node
  and iter_node store seg node =
    iter_view store seg (Node.view context node)
  in
  iter_node [] seg node

let generate_from_view context segs view =
  let rec iter steps_rev view = function
    | [] ->
       begin match view with
       | Node.Leaf (value, _, _) -> Ok {proof_stream=List.rev steps_rev; data=value}
       | _ -> Error `NonExistentPath
       end
    | seg :: segs' ->
       begin match view with
       | Node.Bud (Some node, _, _) ->
          generate_1step_from_node context seg node >>= fun (step, next_view) ->
          iter (step :: steps_rev) next_view segs'
       | _ -> Error `NonExistentPath
       end
  in
  iter [] view segs

let generate cursor segs =
  let Cursor.Cursor (_top, node, context) = Error.from_Ok (Cursor.go_top cursor) in
  generate_from_view context segs  (Node.view context node)
  |> Result.map_error (fun `NonExistentPath -> NonExistentPath segs)

let rec validate_1step seg step last_h =
  match (Segment.cut seg, step) with
  | (None, []) -> (* empty segment *)
     Ok last_h
  | (None, _ :: _) -> Error `NonExistentPath
  | (Some (_, _), []) ->  Error `NonExistentPath
  | (Some (Segment.Left, seg'), ProofInternal h_right :: step') ->
     validate_1step seg' step' last_h >>= fun h_left ->
     let h = Node_hash.of_internal h_left h_right in
     Ok h
  | (Some (Segment.Right, seg'), ProofInternal h_left :: step') ->
     validate_1step seg' step' last_h >>= fun h_right ->
     let h = Node_hash.of_internal h_left h_right in
     Ok h
  | (Some (_, _), ProofExtender prefix :: step') ->
     rm_prefix prefix seg >>= fun rest ->
     validate_1step rest step' last_h >>= fun h ->
     Ok (Node_hash.of_extender prefix h)

let validate_aux segs proof_stream last_h =
  let rec iter h = function
    | ([], []) -> Ok h
    | (seg :: segs_rev, step :: steps_rev) ->
       validate_1step seg step h >>= fun h' ->
       iter (Node_hash.of_bud (Some h')) (segs_rev, steps_rev)
    | (_, _) -> Error `NonExistentPath
  in
  iter last_h (List.rev segs, List.rev proof_stream)

let validate segs proof =
  let last_h = Node_hash.of_leaf proof.data in
  validate_aux segs proof.proof_stream last_h
  |> Result.map_error (fun `NonExistentPath -> NonExistentPath segs)
