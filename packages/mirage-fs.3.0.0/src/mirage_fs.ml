(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

type error = [
  | `Is_a_directory
  | `No_directory_entry
  | `Not_a_directory
]

type write_error = [
  | error
  | `File_already_exists
  | `No_directory_entry
  | `No_space
]

let pp_error ppf = function
  | `Is_a_directory      -> Fmt.string ppf "is a directory"
  | `Not_a_directory     -> Fmt.string ppf "is not a directory"
  | `No_directory_entry  ->
    Fmt.string ppf "a directory in the path does not exist"

let pp_write_error ppf = function
  | #error as e          -> pp_error ppf e
  | `Directory_not_empty -> Fmt.string ppf "directory is not empty"
  | `File_already_exists -> Fmt.string ppf "file already exists"
  | `No_space            -> Fmt.string ppf "device has no more free space"

type stat = {
  filename: string;
  read_only: bool;
  directory: bool;
  size: int64;
}

type fs_error = error
type fs_write_error = write_error

module type S = sig
  type error = private [> fs_error]
  val pp_error: error Fmt.t
  type write_error = private [> fs_write_error]
  val pp_write_error: write_error Fmt.t
  include Mirage_device.S
  val read: t -> string -> int -> int ->
    (Cstruct.t list, error) result Lwt.t
  val size: t -> string -> (int64, error) result Lwt.t
  val create: t -> string -> (unit, write_error) result Lwt.t
  val mkdir: t -> string -> (unit, write_error) result Lwt.t
  val destroy: t -> string -> (unit, write_error) result Lwt.t
  val stat: t -> string -> (stat, error) result Lwt.t
  val listdir: t -> string -> (string list, error) result Lwt.t
  val write: t -> string -> int -> Cstruct.t ->
    (unit, write_error) result Lwt.t
end

module To_KV_RO (FS: S) = struct
  open Lwt.Infix

  type t = FS.t

  type error = [ Mirage_kv.error | `FS of FS.error ]

  type key = Mirage_kv.Key.t

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | `FS e                 -> FS.pp_error ppf e

  let disconnect t = FS.disconnect t

  let exists t key =
    let name = Mirage_kv.Key.to_string key in
    FS.stat t name >|= function
    | Ok s ->
      Ok (Some (if s.directory then `Dictionary else `Value))
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
      FS.read t name 0 (Int64.to_int s.size) >|= function
      | Error e -> Error (`FS e)
      | Ok l -> Ok Cstruct.(to_string (concat l))

  let list t key =
    let name = Mirage_kv.Key.to_string key in
    let dict_or_value fn =
      FS.stat t Mirage_kv.Key.(to_string (key / fn)) >|= function
      | Error e -> Error (`FS e)
      | Ok s -> Ok (if s.directory then `Dictionary else `Value)
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
