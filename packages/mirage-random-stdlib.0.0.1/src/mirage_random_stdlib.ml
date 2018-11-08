(*
 * Copyright (c) 2017,2018 Hannes Mehnert <hannes@mehnert.org>
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

type buffer = Cstruct.t

type g = unit

let generate ?g:_g n =
  let b = Cstruct.create n in
  for i = 0 to pred n do
    Cstruct.set_uint8 b i (Random.int 256)
  done ;
  b

module E = Entropy

let attach e =
  let reseed ~source buffer =
    let data =
      Cstruct.fold (fun acc x -> x :: acc)
        (Cstruct.iter (fun _ -> Some 2) (fun b -> Cstruct.BE.get_uint16 b 0) buffer)
        []
    in
    let array = Array.of_list (source :: data) in
    Random.full_init array in
  E.add_handler e reseed

type t = { e : E.t ; token : E.token }

let active = ref None
and mx     = Lwt_mutex.create ()

let initialize () =
  let open Lwt.Infix in
  Lwt_mutex.with_lock mx @@ fun () ->
    let reg e = attach e >|= fun token -> active := Some { e ; token } in
    match !active with
    | Some _ -> Lwt.return_unit
    | None -> E.connect () >>= reg
