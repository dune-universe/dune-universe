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

open EzCompat

(* type t = string *)

let cut_at_last_extension basename =
  try
    let pos = String.rindex basename '.' in
    EzString.before basename pos,
    String.lowercase (EzString.after basename pos)
  with Not_found -> (basename, "")

let extensions_of_basename basename =
  match EzString.split basename '.' with
    [] | [_] -> []
  | _basename :: exts -> exts

let extensions file = extensions_of_basename (Filename.basename file)

let is_absolute file = not (Filename.is_relative file)
let is_relative = Filename.is_relative
let is_implicit = Filename.is_implicit
let concat = Filename.concat
let add_basename = Filename.concat
let add_basenames = List.fold_left Filename.concat
let dirname = Filename.dirname
let basename = Filename.basename
let check_suffix = Filename.check_suffix
let add_suffix = (^)
let chop_extension = Filename.chop_extension

let last_extension filename =
  let basename = basename filename in
  try
    match List.rev (extensions_of_basename basename) with
    | [] -> None
    | ext :: _ -> Some ext
  with Not_found -> None

let open_in = open_in
let open_in_bin = open_in_bin
let open_out = open_out
let open_out_bin = open_out_bin

let open_fd file mode perm = MinUnix.openfile file mode perm

let temp_file prefix suffix = Filename.temp_file prefix suffix
let current_dir_name = Filename.current_dir_name



let with_open open_channel close_channel filename f =
  let ic = open_channel filename in
  try
    let x = f ic in
    close_channel ic;
    x
  with exn ->
    close_channel ic;
    raise exn

let with_in filename f = with_open open_in close_in filename f
let with_in_bin filename f = with_open open_in_bin close_in filename f

let with_out = with_open open_out close_out
let with_out_bin = with_open open_out_bin close_out

let copy_file f1 f2 =
  with_in_bin f1 (fun ic ->
      with_out_bin f2 (fun oc ->
          FileChannel.copy_file ic oc))

let iter_blocks f filename =
  with_in_bin filename (fun ic ->
      FileChannel.iter_blocks f ic)

let write_file filename str =
  with_out_bin filename (fun oc ->
      output_string oc str)
let file_of_string = write_file

let read_file filename =
  with_in_bin filename FileChannel.read_file
let string_of_file = read_file


let read_lines filename =
  with_in filename FileChannel.read_lines
let read_lines_to_revlist filename =
  with_in filename FileChannel.read_lines_to_revlist
let read_lines_to_list filename =
  with_in filename FileChannel.read_lines_to_list

let write_lines filename lines =
  with_out filename (fun oc -> FileChannel.write_lines oc lines)
let write_lines_of_list filename lines =
  with_out filename (fun oc -> FileChannel.write_lines_of_list oc lines)

let lines_of_file = read_lines
let file_of_lines = write_lines

let iter_lines f filename =
  with_in filename (fun ic ->
      FileChannel.iter_lines f ic)

let iteri_lines f filename =
  with_in filename (fun ic ->
      FileChannel.iteri_lines f ic)

let read_sublines filename off len =
  with_in filename (fun ic ->
      FileChannel.read_sublines ic off len)
let read_sublines_to_list filename off len =
  with_in filename (fun ic ->
      FileChannel.read_sublines_to_list ic off len)

let read_subfile filename pos len =
  with_in filename (fun ic ->
      FileChannel.read_subfile ic pos len)

let string_of_subfile = read_subfile







let is_directory = Sys.is_directory

let is_link filename =
  try let s = MinUnix.lstat filename in
    s.MinUnix.st_kind = MinUnix.S_LNK with _ -> false

let rename = Sys.rename
let remove = Sys.remove
let getcwd = Sys.getcwd
let exists = Sys.file_exists
let stat filename = MinUnix.stat filename
let lstat filename = MinUnix.lstat filename

let to_string s = s

let size filename =
  let s = MinUnix.stat filename in
  s.MinUnix.st_size

(*
let size64 filename =
  let s = MinUnix.LargeFile.stat filename in
  s.MinUnix.LargeFile.st_size
*)

module OP = struct

  let (//) = Filename.concat

  end

module Directory_operations = FileDirMaker.Make(struct
    type path = string
    let add_basename = add_basename
    let dirname = dirname
    let basename = basename

    let rmdir = MinUnix.rmdir
    let lstat = MinUnix.lstat
    let stat = MinUnix.stat
    let mkdir = MinUnix.mkdir

    let remove = Sys.remove
    let readdir = Sys.readdir

    let to_string = to_string
  end)

include Directory_operations


(* [dst] must be the target file name, not the name of its
   directory *)
let rec copy_rec src dst =
  (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
  match (MinUnix.stat src).MinUnix.st_kind with
  | MinUnix.S_DIR ->
    make_dir ~p:true dst;
    iter_dir (fun ~basename ~localpath:_ ~file:_ ->
        copy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src
  | MinUnix.S_REG ->
    copy_file src dst
  | _ ->
    failwith (Printf.sprintf
                "File.copy_rec: cannot copy unknown kind file %S"
                src)

  (* [dst] must be the target file name, not the name of its directory *)
let rec uncopy_rec src dst =
  match
    (try Some (MinUnix.stat src).MinUnix.st_kind with _ -> None),
    (try Some (MinUnix.stat dst).MinUnix.st_kind with _ -> None)
  with
  | _, None -> ()
  | Some MinUnix.S_DIR, Some MinUnix.S_DIR ->
    iter_dir (fun ~basename ~localpath:_ ~file:_ ->
        uncopy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src;
    (try MinUnix.rmdir dst with _ -> ())
  | Some MinUnix.S_REG, Some MinUnix.S_REG ->
    Sys.remove dst
  | _ ->
    failwith (Printf.sprintf
                "File.uncopy_rec: inconsistent kinds between %S and %S"
                src dst)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
    [] -> raise Not_found
      | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end
