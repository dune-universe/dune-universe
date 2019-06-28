(*
 * Copyright (c) 2015 Thomas Leonard
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

open Lwt.Infix

module OS = Os_xen
module Gntref = OS.Xen.Gntref
module Export = OS.Xen.Export

let return = Lwt.return

let max_pages = 256

type block = {
  id : Cstruct.uint16;
  gref : Gntref.t;
  data : Cstruct.t;
}
type t = {
  grant : Gntref.t -> Io_page.t -> unit;
  mutable next_id : Cstruct.uint16;
  mutable blocks : block list;
  mutable in_use : int;
  mutable shutdown : bool;
  avail : unit Lwt_condition.t;  (* Fires when free list becomes non-empty *)
}

let page_size = Io_page.round_to_page_size 1
let block_size = page_size / 2

let make grant = { next_id = 0; grant; blocks = []; shutdown = false; in_use = 0; avail = Lwt_condition.create () }

let shutdown t =
  t.shutdown <- true;
  Lwt_condition.broadcast t.avail ();   (* Wake anyone who's still waiting for free pages *)
  if t.in_use = 0 then (
    t.blocks |> List.iter (fun {id = _; gref; data} ->
      if data.Cstruct.off = 0 then (
        Lwt.async (fun () -> Export.end_access ~release_ref:true gref)
      )
    );
    t.blocks <- []
  )
  (* Otherwise, shutdown gets called again when in_use becomes 0 *)

let alloc t =
  let page = Io_page.get 1 in
  (* (the Xen version of caml_alloc_pages clears the page, so we don't have to) *)
  Export.get () >>= fun gnt ->
  t.grant gnt page;
  return (gnt, Io_page.to_cstruct page)

let put t block =
  let was_empty = (t.blocks = []) in
  t.blocks <- block :: t.blocks;
  t.in_use <- t.in_use - 1;
  if was_empty then Lwt_condition.broadcast t.avail ();
  if t.in_use = 0 && t.shutdown then shutdown t

let use_block t fn block =
  let {id; gref; data} = block in
  t.in_use <- t.in_use + 1;
  Lwt.try_bind
    (fun () -> fn ~id gref data)
    (fun (_, release as result) ->
      Lwt.on_termination release (fun () -> put t block);
      return result
    )
    (fun ex -> put t block; Lwt.fail ex)

let rec use t fn =
  if t.shutdown then
    failwith "Shared_page_pool.use after shutdown";
  match t.blocks with
  | [] when t.next_id >= max_pages ->
      MProf.Trace.label "Shared_page_pool waiting for free";
      Lwt_condition.wait t.avail >>= fun () -> use t fn
  | [] ->
      (* Frames normally fit within 2048 bytes, so we split each page in half. *)
      alloc t >>= fun (gref, page) ->
      let b1 = Cstruct.sub page 0 block_size in
      let b2 = Cstruct.shift page block_size in
      let id1 = t.next_id in
      let id2 = t.next_id + 1 in
      t.next_id <- t.next_id + 2;
      t.blocks <- {id = id2; gref; data = b2} :: t.blocks;
      Lwt_condition.broadcast t.avail ();
      use_block t fn {id = id1; gref; data = b1}
  | hd :: tl ->
      t.blocks <- tl;
      use_block t fn hd

let blocks_needed bytes =
  (bytes + block_size - 1) / block_size
