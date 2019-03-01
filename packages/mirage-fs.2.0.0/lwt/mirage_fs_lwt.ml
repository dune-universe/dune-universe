(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
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

module type S = Mirage_fs.S
  with type 'a io = 'a Lwt.t
   and type page_aligned_buffer = Cstruct.t

module To_KV_RO (FS: S) = struct

  type t = FS.t
  type +'a io = 'a Lwt.t

  type error = [ Mirage_kv.error | `FS of FS.error ]

  type key = Mirage_kv.Key.t
  type value = string

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | `FS e                 -> FS.pp_error ppf e

  let disconnect t = FS.disconnect t

  let exists t key =
    let name = Mirage_kv.Key.to_string key in
    FS.stat t name >|= function
    | Ok s ->
      Ok (Some (if s.Mirage_fs.directory then `Dictionary else `Value))
    | Error `Not_a_directory
    | Error `No_directory_entry -> Ok None
    | Error e -> Error (`FS e)

  let get t key =
    let name = Mirage_kv.Key.to_string key in
    FS.stat t name >>= function
    | Error `Is_a_directory -> Lwt.return (Error (`Value_expected key))
    | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
    | Error e -> Lwt.return (Error (`FS e))
    | Ok s ->
      FS.read t name 0 (Int64.to_int s.Mirage_fs.size) >|= function
      | Error e -> Error (`FS e)
      | Ok l -> Ok Cstruct.(to_string (concat l))

  let list t key =
    let name = Mirage_kv.Key.to_string key in
    let dict_or_value fn =
      FS.stat t Mirage_kv.Key.(to_string (key / fn)) >|= function
      | Error e -> Error (`FS e)
      | Ok s -> Ok (if s.Mirage_fs.directory then `Dictionary else `Value)
    in
    FS.listdir t name >>= function
    | Error `Not_a_directory -> Lwt.return (Error (`Dictionary_expected key))
    | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
    | Error e -> Lwt.return (Error (`FS e))
    | Ok files ->
      Lwt_list.fold_left_s (fun acc f ->
          match acc with
          | Error e -> Lwt.return (Error e)
          | Ok acc -> dict_or_value f >|= function
            | Error e -> Error e
            | Ok t -> Ok ((f, t) :: acc))
        (Ok []) files

  let last_modified _ _ = Lwt.return (Ok (0, 0L))

  let digest t key =
    get t key >|= function
    | Error e -> Error e
    | Ok data -> Ok (Digest.string data)

  let connect t = Lwt.return t

end
