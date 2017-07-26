(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
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
 *
 *)

module IntSet = Set.Make(struct type t = int let compare (a: int) (b: int) = compare a b end)

(* try to minimise id reuse by forcing ourselves to cycle through all the free_ids
   before re-using them *)
type t = {
  mutable free_ids: IntSet.t; (* use these first *)
  mutable used_ids: IntSet.t; (* when free_ids are exhausted, take these back *)
  free_ids_c: unit Lwt_condition.t;
}

let make ?(max_id = 512) () =
  let free_ids =
    let rec loop acc = function
    | 0 -> acc
    | n -> loop (IntSet.add n acc) (n - 1) in
    loop IntSet.empty max_id in
  let used_ids = IntSet.empty in
  let free_ids_c = Lwt_condition.create () in
  { free_ids; used_ids; free_ids_c }

let rec with_id t f =
  let open Lwt.Infix in
  if t.free_ids = IntSet.empty then begin
    if t.used_ids = IntSet.empty then begin
      Lwt_condition.wait t.free_ids_c
      >>= fun () ->
      with_id t f
    end else begin
      (* start reusing old ids now *)
      t.free_ids <- t.used_ids;
      t.used_ids <- IntSet.empty;
      with_id t f
    end
  end else begin
    let free_id = IntSet.min_elt t.free_ids in
    t.free_ids <- IntSet.remove free_id t.free_ids;
    Lwt.finalize
      (fun () -> f free_id)
      (fun () ->
         t.used_ids <- IntSet.add free_id t.used_ids;
         Lwt_condition.signal t.free_ids_c ();
         Lwt.return_unit
      )
  end
