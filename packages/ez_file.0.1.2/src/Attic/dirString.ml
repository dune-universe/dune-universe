(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open StringCompat

(*
let mkdir dir perm = MinUnix.mkdir (to_string dir) perm
let make dir = mkdir dir 0o755

let rec make_all dir =
  if exists dir then begin
    if not (is_directory dir) then
      failwith (Printf.sprintf "File.Dir.make_all: %s not a directory" (to_string dir))
  end
  else
  if is_link dir then
    failwith (Printf.sprintf "File.Dir.make_all: %s is an orphan symbolic link" (to_string dir))
  else begin
    let predir = dirname dir in
    if predir != dir then make_all predir;
    if not (exists dir) then
      try
        mkdir dir 0o775
      with e ->
        failwith (Printf.sprintf "File.Dir.make_all: mkdir [%s] raised %s" (to_string dir) (Printexc.to_string e))
  end

  let list filename = Array.to_list (Sys.readdir (to_string filename))

  let list_files filename =
    Array.to_list (
      Array.map (fun file -> add_basename filename file)
        (Sys.readdir (to_string filename)))

  let iter f dirname =
    Array.iter f (Sys.readdir (to_string dirname))

  let iter_files f dirname =
    List.iter f (list_files dirname)

  let remove dir = MinUnix.rmdir (to_string dir)

let rec remove_all (dir : t) =
  iter_files (fun filename ->
      if not (X.is_link filename) && X.is_directory filename then
        remove_all filename
      else
        X.remove filename
    ) dir;
  remove dir
*)
