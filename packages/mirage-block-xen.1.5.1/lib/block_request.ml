(*
 * Copyright (c) 2014 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Blkproto.Req

type request = {
  id: int64 list;
  op: op;
  sector: int64;
  length: int;
  buffers: Cstruct.t list;
  depends: int64 list;
}

let string_of_request r =
  let int64 x = Int64.to_string x in
  let int x = string_of_int x in
  let list ty xs = String.concat "; " (List.map ty xs) in
  Printf.sprintf "{ id = [ %s ]; op = %s; sector = %Ld; length = %d; buffers = [ %s ]; depends = [ %s ]}"
    (list int64 r.id) (string_of_op r.op) r.sector r.length (list int (List.map Cstruct.len r.buffers)) (list int64 r.depends)

type t = request list

let empty = [] (* in reverse order *)

let conflicts a b = match a.op, b.op with
  | Read, Read -> false
  | _, _ ->
    let open Int64 in
    (* Allow writes to complete out of order: this relies on the higher
       level software not issuing critically-ordered writes in parallel *)
    not (add a.sector (of_int a.length) < b.sector
     || (add b.sector (of_int b.length) < a.sector))

let add t id op sector buffers =
  let length = List.fold_left (+) 0 (List.map Cstruct.len buffers) / 512 in
  let r = { id = [id]; op; sector; length; buffers; depends = [] } in
  let depends = List.(concat (map (fun r -> r.id) (filter (conflicts r) t))) in
  let r = { r with depends } in
  r :: t

let coalesce requests =
  (* merge adjacent request structures *)
  let rec reqs finished offset current = function
  | [] -> List.rev (if current = [] then finished else (List.rev current) :: finished)
  | r :: rs when r.sector = offset -> reqs finished (Int64.(add offset (of_int r.length))) (r :: current) rs
  | r :: rs -> reqs (if current = [] then finished else current :: finished) (Int64.(add r.sector (of_int r.length))) [ r ] rs in
  (* merge adjacent cstruct buffers *)
  let rec merge_buffers finished current = function
  | [] -> List.rev (if Cstruct.len current = 0 then finished else current :: finished)
  | b :: bs when current.Cstruct.len <> 0
            && current.Cstruct.buffer == b.Cstruct.buffer
            && (current.Cstruct.off + current.Cstruct.len = b.Cstruct.off) ->
    merge_buffers finished (Cstruct.add_len current b.Cstruct.len) bs
  | b :: bs -> merge_buffers (if Cstruct.len current = 0 then finished else current :: finished) b bs in
  let merge requests =
    let batches = reqs [] (-1L) [] requests in
    List.map (function
    | [] -> []
    | r :: rs -> [ { r with id = List.concat (List.map (fun r -> r.id) (r :: rs));
                            length = List.fold_left (+) 0 (List.map (fun r -> r.length) (r :: rs));
                            buffers = merge_buffers [] (Cstruct.create 0) (List.concat (List.map (fun r -> r.buffers) (r :: rs))) } ]
    ) batches in
  let sorted = List.sort (fun a b -> compare a.sector b.sector) requests in
  let reads = List.filter (fun r -> r.op = Read) sorted in
  let writes = List.filter (fun r -> r.op = Write) sorted in
  List.concat (merge reads @ (merge writes))

let pop t =
  let nodeps, deps = List.partition (fun t -> t.depends = []) t in
  let nodeps_ids = List.(concat (map (fun t -> t.id) nodeps)) in
  let deps = List.map (fun t -> { t with depends = List.filter (fun x -> not(List.mem x nodeps_ids)) t.depends }) deps in
  let nodeps = List.rev nodeps in
  coalesce nodeps, deps
