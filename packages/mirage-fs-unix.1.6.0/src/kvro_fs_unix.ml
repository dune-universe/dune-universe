(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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

type +'a io = 'a Lwt.t
type page_aligned_buffer = Cstruct.t

type error = [ Mirage_kv.error | FS_common.error ]

let pp_error ppf = function
  | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
  | #FS_common.error as e -> FS_common.pp_error ppf e

type t = {
  base: string
}

let connect id =
  (* TODO verify base directory exists *)
  Lwt.return ({ base=id })

let disconnect _ = Lwt.return ()

let remap name = function
  | Error `No_directory_entry -> Error (`Unknown_key name)
  | Error e -> Error (e :> error)
  | Ok l -> Ok l

let mem {base} name =
  FS_common.mem_impl base name >|= remap name

let read {base} name off len =
  let i = Int64.to_int in
  FS_common.read_impl base name (i off) (i len) >|= remap name

let size {base} name =
  FS_common.size_impl base name >|= remap name
