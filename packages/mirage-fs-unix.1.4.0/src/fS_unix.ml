(*
 * Copyright (c) 2013-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014      Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014      Hannes Mehnert <hannes@mehnert.org>
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
open Result

type +'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

type t = {
  base: string
}

type fs_error = FS_common.fs_error
type error = FS_common.error
type write_error = FS_common.write_error
let pp_error = FS_common.pp_error
let pp_write_error = FS_common.pp_write_error

let disconnect _ = Lwt.return ()

let read {base} name off len =
  FS_common.read_impl base name off len

let size {base} name =
  FS_common.size_impl base name

(* all mkdirs are mkdir -p *)
let rec create_directory path =
  let check_type path =
    Lwt_unix.LargeFile.stat path >>= fun stat ->
    match stat.Lwt_unix.LargeFile.st_kind with
    | Lwt_unix.S_DIR -> Lwt.return (Ok ())
    | _ -> Lwt.return (Error `File_already_exists)
  in
  if Sys.file_exists path then check_type path
  else begin
    create_directory (Filename.dirname path) >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      Lwt.catch (fun () ->
        Lwt_unix.mkdir path 0o755 >>= fun () -> Lwt.return (Ok ())
      )
      (function
        | Unix.Unix_error (ex, _, _) -> Lwt.return (FS_common.map_write_error ex)
        | e -> Lwt.fail e
      )
  end

let mkdir {base} path =
  let path = FS_common.resolve_filename base path in
  create_directory path

let open_file base path flags =
  let path = FS_common.resolve_filename base path in
  create_directory (Filename.dirname path) >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    Lwt.catch (fun () -> Lwt_unix.openfile path flags 0o644 >|= fun fd -> Ok fd)
    (function
      | Unix.Unix_error (ex, _, _) -> Lwt.return (FS_common.map_write_error ex)
      | e -> Lwt.fail e)

let create {base} path =
  open_file base path [Lwt_unix.O_CREAT] >>= function
  | Ok fd ->
    Lwt_unix.close fd >|= fun () ->
    Ok ()
  | Error e -> Lwt.return (Error e)

let stat {base} path0 =
  let path = FS_common.resolve_filename base path0 in
  Lwt.catch (fun () ->
    Lwt_unix.LargeFile.stat path >>= fun stat ->
    let size = stat.Lwt_unix.LargeFile.st_size in
    let filename = Filename.basename path in
    let read_only = false in
    let directory = Sys.is_directory path in
    Lwt.return (Ok { Mirage_fs.filename; read_only; directory; size })
  )
  (fun e -> FS_common.err_catcher e)

let connect id =
  try if Sys.is_directory id then
      Lwt.return ({base = id})
    else
      Lwt.fail_with ("Not a directory " ^  id)
  with Sys_error _ -> Lwt.fail_with ("Not an entity " ^ id)

let list_directory path =
  if Sys.file_exists path then (
    let s = Lwt_unix.files_of_directory path in
    let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
    Lwt_stream.to_list s >>= fun l ->
    Lwt.return (Ok l)
  ) else
    Lwt.return (Ok [])


let listdir {base} path =
  let path = FS_common.resolve_filename base path in
  list_directory path

let remove path =
  let rec rm rm_top_dir base p =
    let full = Filename.concat base p in
    Lwt_unix.LargeFile.stat full >>= fun stat ->
    match stat.Lwt_unix.LargeFile.st_kind with
    | Lwt_unix.S_DIR ->
      list_directory full >>= (function
          | Ok files ->
            Lwt_list.iter_p (rm true full) files >>= fun () ->
            if rm_top_dir then Lwt_unix.rmdir full else Lwt.return_unit
          | Error e -> Lwt.fail e)
    | Lwt_unix.S_REG | Lwt_unix.S_LNK -> Lwt_unix.unlink full
    | _ -> Lwt.fail (Failure "cannot remove this file, as its type is unknown")
  in
  Lwt.catch (fun () -> rm false (Filename.dirname path) (Filename.basename path) >|= fun () -> Ok ())
  (fun e -> FS_common.write_err_catcher e)

let destroy {base} path =
  let path = FS_common.resolve_filename base path in
  remove path

let write {base} path off buf =
  open_file base path Lwt_unix.([O_WRONLY; O_NONBLOCK; O_CREAT]) >>= function
  | Error e -> Lwt.return (Error e)
  | Ok fd ->
    Lwt.catch
      (fun () ->
         Lwt_unix.lseek fd off Unix.SEEK_SET >>= fun _seek ->
         let buf = Cstruct.to_string buf in
         let rec aux off remaining =
           if remaining = 0 then
             Lwt_unix.close fd
           else (
             Lwt_unix.write fd buf off remaining >>= fun n ->
             aux (off+n) (remaining-n))
         in
         aux 0 (String.length buf) >>= fun () ->
         Lwt.return (Ok ()))
      (fun e ->
         Lwt_unix.close fd >>= fun () ->
         FS_common.write_err_catcher e
      )
