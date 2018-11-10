(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let split_string delimiter name =
  let len = String.length name in
  let rec doit off acc =
    let open String in
    let idx = try index_from name off delimiter with _ -> len in
    let fst = sub name off (idx - off) in
    let idx' = idx + 1 in
    if idx' <= len then
      doit idx' (fst :: acc)
    else
      fst :: acc
  in
  List.rev (doit 0 [])

let rec remove_dots parts outp =
  match parts, outp with
  | ".."::r, _::rt -> remove_dots r  rt
  | ".."::r, []    -> remove_dots r  []
  | "."::r , rt    -> remove_dots r  rt
  | r::rs  , rt    -> remove_dots rs (r :: rt)
  | []     , rt    -> List.rev rt

let resolve_filename base filename =
  let parts = split_string '/' filename in
  let name = remove_dots parts [] |> String.concat "/" in
  Filename.concat base name

type fs_error = [
  | `Unix_error of Unix.error
  | `Unix_errorno of int
  | `Negative_bytes
]

type error = [ Mirage_fs.error | fs_error ]
type write_error = [ Mirage_fs.write_error | fs_error | `Directory_not_empty ]

let pp_fs_error ppf = function
  | `Unix_errorno i -> Fmt.pf ppf "UNIX errorno: %d" i
  | `Unix_error e   -> Fmt.pf ppf "UNIX error: %s" @@ Unix.error_message e
  | `Negative_bytes -> Fmt.string ppf "can't read negative bytes"

let pp_error ppf = function
  | #Mirage_fs.error as e -> Mirage_fs.pp_error ppf e
  | #fs_error as e        -> pp_fs_error ppf e

let pp_write_error ppf = function
  | #Mirage_fs.write_error as e -> Mirage_fs.pp_write_error ppf e
  | #fs_error as e              -> pp_fs_error ppf e
  | `Directory_not_empty        -> Fmt.string ppf "XXX"

let map_error = function
  | Unix.EISDIR -> Error `Is_a_directory
  | Unix.ENOENT -> Error `No_directory_entry
  | Unix.EUNKNOWNERR i -> Error (`Unix_errorno i)
  | e -> Error (`Unix_error e)

let map_write_error: Unix.error -> ('a, write_error) result = function
  | Unix.EEXIST -> Error `File_already_exists
  | Unix.EISDIR -> Error `Is_a_directory
  | Unix.ENOENT -> Error `No_directory_entry
  | Unix.ENOSPC -> Error `No_space
  | Unix.ENOTEMPTY -> Error `Directory_not_empty
  | Unix.EUNKNOWNERR i -> Error (`Unix_errorno i)
  | e -> Error (`Unix_error e)

let err_catcher = function
  | Unix.Unix_error (ex, _, _) -> Lwt.return (map_error ex)
  | exn -> Lwt.fail exn

let write_err_catcher = function
  | Unix.Unix_error (ex, _, _) -> Lwt.return (map_write_error ex)
  | exn -> Lwt.fail exn

let mem_impl base name =
  let fullname = resolve_filename base name in
  Lwt.catch (fun () ->
          Lwt_unix.stat fullname >>= fun _ -> Lwt.return (Ok true)
        )
  (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok false)
          | e -> err_catcher e)

let read_impl base name off reqd_len: (Cstruct.t list, error) result Lwt.t =
  if reqd_len < 0 then Lwt.return (Error `Negative_bytes)
  else begin
    let fullname = resolve_filename base name in
    Lwt.catch (fun () ->
        Lwt_unix.openfile fullname [Lwt_unix.O_RDONLY] 0 >>= fun fd ->
        Lwt_unix.lseek fd off Lwt_unix.SEEK_SET >>= fun _seek -> (* very little we can do with _seek *)
        let st =
          Lwt_stream.from (fun () ->
              let buf = Cstruct.create 4096 in
              Lwt_cstruct.read fd buf >>= fun len ->
              match len with
              | 0 ->
                Lwt_unix.close fd
                >>= fun () -> Lwt.return None
              | len -> Lwt.return (Some (Cstruct.sub buf 0 (min len reqd_len)))
            )
        in
        Lwt_stream.to_list st >|= fun bufs ->
        match reqd_len with
        | 0 -> Ok []
        | _ -> Ok bufs)
      err_catcher
  end

let size_impl base name =
  let fullname = resolve_filename base name in
  Lwt.catch (fun () ->
    Lwt_unix.LargeFile.stat fullname >|= fun stat ->
    match stat.Lwt_unix.LargeFile.st_kind with
      | Lwt_unix.S_REG -> Ok stat.Lwt_unix.LargeFile.st_size
      | _ -> Error `Is_a_directory)
    err_catcher
