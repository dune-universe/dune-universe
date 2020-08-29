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

(* Currently, we have no distinction between implicit and .-relative
   filenames. We should probably do that.
*)

open EzCompat


(* IMPORTANT OS specificities (from the JDK):

   Each filename is composed of:
   - an optional prefix:
   nothing
   / root on Unix for absolute filenames
   \ root on Windows for absolute filenames without drive
   \\ UNC filename
   c: drive on Windows for relative filenames
   c:\ root and drive on windows for absolute filenames
   (nothing on Unix or c: or C: on Windows or \)
   - a list of basenames (possibly empty for the root )

   - there is an official separator like \ or /
   - there is an official path-separator like : (unix) or ; (windows)

   - listRoots() returns the list of available drives
   - getAbsolutePath() -> absolute path
   - getCanonicalPath() -> absolute path simplified and without symlinks

*)

(*
  [file_dir] points to the parent directory, unless when it points back to
   the file itself, which happens for:
  * the File.t is a root directory, with [file_basename] equals "/" or "\\"
  * the File.t is the current directory, with [file_basename] equals "."
  * the File.t is the parent directory, with [file_basename] equals ".."
  * the File.t is implicit
*)

type t = {
  file_basename : string;
  file_dir : t;
  file_string : string;
  (* The system filename, i.e. with system-specific separators *)
  file_partition : string;
}

let root_basename = FileOS.dir_separator_string

let basename t = t.file_basename

let rec is_absolute t =
  if t.file_dir != t then
    is_absolute t.file_dir
  else
    t.file_basename = root_basename

let is_relative t = not (is_absolute t)
let is_implicit t =
  if t.file_dir == t then
    match t.file_basename with
    | "/" | "\\" -> false
    | _ -> true
  else
    let rec iter t =
    if t.file_dir != t then
      iter t.file_dir
    else
      match t.file_basename with
      | "." | ".." | "/" | "\\" -> false
      | _ -> true
  in
  iter t.file_dir

       (*
let to_root_dir t =
  let rec root = {
    file_dir = root;
    file_basename = root_basename;
    file_partition = t.file_partition;
    file_string = t.file_partition ^ root_basename;
  } in
  root
        *)

let to_empty_dir t =
  let rec root = {
    file_dir = root;
    file_basename = "";
    file_partition = t.file_partition;
    file_string = t.file_partition;
  } in
  root

let to_current_dir t =
  let rec root = {
    file_dir = root;
    file_basename = ".";
    file_partition = t.file_partition;
    file_string = t.file_partition ^ ".";
  } in
  root

let to_parent_dir t =
  let rec root = {
    file_dir = root;
    file_basename = "..";
    file_partition = t.file_partition;
    file_string = t.file_partition ^ "..";
  } in
  root

    (*
let rec to_string_raw t =
  if t.file_dir == t then
    Printf.sprintf "[%s]" t.file_basename
  else
    Printf.sprintf "%s::%s" (to_string_raw t.file_dir) t.file_basename
     *)

let to_string t = t.file_string  (* ^ "=" ^ to_string_raw t *)
let dirname t =
  if t.file_dir == t then
    match t.file_basename with
    | "/" | "\\" | "." -> t
    | _ -> to_current_dir t
  else t.file_dir

let add_basename_string dir basename =
  match dir.file_basename with
  | "" | "/" | "\\" ->
    dir.file_partition ^ dir.file_basename ^ basename
  | _ ->
    dir.file_string ^ FileOS.dir_separator_string ^ basename

let add_basename_simple dir basename =
  {
    file_basename =  basename;
    file_dir = dir;
    file_partition = dir.file_partition;
    file_string = add_basename_string dir basename;
  }

let add_basename dir basename =
  match dir.file_basename, basename with
  | "..", ".." -> add_basename_simple dir basename
  | (""|"."), ".." -> to_parent_dir dir
  | ("/" | "\\"), ".." -> dir
  | _, ".." ->
    if dir.file_dir == dir then to_empty_dir dir else dir.file_dir
  | _, ("."|"/"|"\\"|"") -> dir
  | _ -> add_basename_simple dir basename

let rec add_basenames dir list =
  match list with
    [] -> dir
  | ("" | ".") :: tail -> add_basenames dir tail
  | basename :: tail -> add_basenames (add_basename dir basename) tail


let concat t1 t2 =
  (*
  if t1.file_partition <> "" &&
     t2.file_partition <> "" &&
     t1.file_partition <> t2.file_partition then
    failwith "Filename2.concat: filenames have different partitions";
  if is_absolute t2 then
    failwith "Filename2.concat: second filename is absolute";
*)
  let rec iter dir t =
    let dir =
      if t.file_dir != t then
        iter dir t.file_dir
      else dir in
    add_basename dir t.file_basename
  in
  iter t1 t2

let check_suffix file suffix =
  Filename.check_suffix file.file_basename suffix

let add_suffix t suffix =
  match t.file_basename with
    "." | ".." | "" -> failwith "Filename2.add_extension: symbolic file"
  | _ ->
    if t.file_dir == t then
      let rec root = {
        file_basename = t.file_basename ^ suffix;
        file_partition = t.file_partition;
        file_dir = root;
        file_string = t.file_string ^ suffix;
      }
      in
      root
    else
      {
        file_basename = t.file_basename ^ suffix;
        file_partition = t.file_partition;
        file_dir = t.file_dir;
        file_string = t.file_string ^ suffix;
      }


(* utilities for [of_string] *)

let rec normalize_path path =
  match path with
    [] -> []
  | dir :: tail ->
    let dir = dir :: normalize_path tail in
    match dir with
    | "" :: path -> path
    | "." :: path -> path
    | ".." :: _ -> dir
    | _ :: ".." :: path -> path
    | _ -> dir

let rec remove_leading_dotdots path =
  match path with
    ".." :: path -> remove_leading_dotdots path
  | _ -> path

let rec make dir path =
  match path with
    [] -> dir
  | basename :: tail ->
    let t = {
      file_basename = basename;
      file_dir = dir;
      file_partition =  dir.file_partition;
      file_string = add_basename_string dir basename;
    } in
    make t tail

type kind =
  | Absolute
  | Current
  | Parent
  | Relative

let of_path part path =
  let kind = match path with
    | "" :: _ :: _ -> Absolute
    | "." :: _ -> Current
    | ".." :: _ -> Parent
    | _ -> Relative
  in
  let path = normalize_path path in
  let path = if kind = Absolute  then remove_leading_dotdots path else path in

  if kind = Absolute then
    let rec root = {
      file_basename = FileOS.dir_separator_string;
      file_dir = root;
      file_string = part ^ FileOS.dir_separator_string;
      file_partition = part;
    } in
    make root path
  else
    match path with
      [] ->
      begin
        match kind with
        | Current | Relative ->
          let basename = if kind = Current then "." else "" in
          let rec current_dir = {
            file_basename = basename;
            file_dir = current_dir;
            file_string = part ^ basename;
            file_partition = part;
          } in
          current_dir
        | _ -> assert false
        end
    | dir :: tail ->
      let rec base_dir = {
        file_basename = dir;
        file_dir = base_dir;
        file_partition = part;
        file_string = part ^ dir;
      } in
      make base_dir tail

let of_unix_string s =
  let path = EzString.split s '/' in
  let part = "" in
  of_path part path


let of_win32_string s =
  let s1, s2  = EzString.cut_at s ':' in
  let ss = if s1 == s then s else s2 in
  let ss = String.map (function '/' -> '\\' | c -> c) ss in
  let part = if s1 == s then "" else (String.lowercase s1) ^ ":" in
  let path = EzString.split ss '\\' in
  of_path part path

let of_string s =
  if FileOS.win32 then of_win32_string s else of_unix_string s





(*
let () =
  let root_dir = of_unix_string "/" in
  let current_dir = of_unix_string "." in
  let parent_dir = of_unix_string ".." in
  let empty_dir = of_unix_string "" in
  let relative_dir = add_basename current_dir "foo" in
  let parrel_dir = add_basename parent_dir "foo" in
  let absolute_dir = add_basename root_dir "foo" in
  let implicit_dir = of_unix_string "foo" in
  let relative_dir2 = add_basenames current_dir ["foo"; "bar"] in
  let parrel_dir2 = add_basenames parent_dir ["foo"; "bar" ] in
  let absolute_dir2 = add_basenames root_dir ["foo"; "bar"] in
  let implicit_dir2 = add_basename (of_unix_string "foo") "bar" in
  let parrel_dir3 = add_basenames parent_dir [".."; "foo"; "bar" ] in

  let oc = open_out "file.result" in

  let test_to_string d =
    Printf.fprintf oc "[%s]\n%!" (to_string d)
  in
  let test_is_absolute d =
    Printf.fprintf oc "is_absolute(%s) = %b\n%!"
      (to_string d) (is_absolute d)
  in
  let test_is_relative d =
    Printf.fprintf oc "is_relative(%s) = %b\n%!"
      (to_string d) (is_relative d)
  in
  let test_is_implicit d =
    Printf.fprintf oc "is_implicit(%s) = %b\n%!"
      (to_string d) (is_implicit d)
  in
  let test_concat d1 d2 =
    Printf.fprintf oc "concat [%s] [%s] = %!"
      (to_string d1) (to_string d2);
      try
        let f = concat d1 d2 in
        Printf.fprintf oc "[%s]\n%!" (to_string f)
      with exn ->
        Printf.fprintf oc "%s\n%!" (Printexc.to_string exn);
  in
  let dirs = [ root_dir;
               current_dir;
               parent_dir;
               empty_dir;
               relative_dir;
               absolute_dir;
               implicit_dir;
               parrel_dir;
               relative_dir2;
               absolute_dir2;
               implicit_dir2;
               parrel_dir2;
               parrel_dir3;
             ] in
  List.iter test_to_string dirs;
  List.iter test_is_absolute dirs;
  List.iter test_is_relative dirs;
  List.iter test_is_implicit dirs;
  List.iter (fun d1 ->
      List.iter (test_concat d1) dirs) dirs;

  close_out oc;

  assert (to_string root_dir = "/");
  assert (to_string current_dir = ".");
  assert (to_string parent_dir = "..");
  assert (to_string relative_dir = "./foo");
  assert (to_string absolute_dir = "/foo");
  assert (to_string implicit_dir = "foo");
  assert (to_string empty_dir = "");
  assert (to_string relative_dir2 = "./foo/bar");
  assert (to_string absolute_dir2 = "/foo/bar");
  assert (to_string implicit_dir2 = "foo/bar");

  assert (is_absolute root_dir);
  assert (not (is_absolute current_dir));
  assert (not (is_absolute parent_dir));
  assert (not (is_absolute relative_dir));
  assert (is_absolute absolute_dir);
  assert (not (is_absolute empty_dir));
  assert (not (is_absolute implicit_dir));

  assert (not (is_relative root_dir));
  assert (is_relative current_dir);
  assert (is_relative parent_dir);
  assert (is_relative relative_dir);
  assert (not (is_relative absolute_dir));
  assert (is_relative empty_dir); (* different from Filename.is_relative "" *)
  assert (is_relative implicit_dir);
  ()
*)

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

let with_in filename f = FileString.with_in (to_string filename) f
let with_in_bin filename f = FileString.with_in_bin (to_string filename) f

let with_out filename f = FileString.with_out (to_string filename) f
let with_out_bin filename f = FileString.with_out_bin (to_string filename) f

let read_sublines file off len =
  FileString.read_sublines (to_string file) off len
let read_sublines_to_list file off len =
  FileString.read_sublines_to_list (to_string file) off len

let iteri_lines f file = FileString.iteri_lines f (to_string file)
let iter_lines f file = FileString.iter_lines f (to_string file)
let write_file file s = FileString.write_file (to_string file) s
let read_file file = FileString.read_file (to_string file)
let write_lines file lines =
  FileString.write_lines (to_string file) lines
let write_lines_of_list file lines =
  FileString.write_lines_of_list (to_string file) lines
let read_lines file = FileString.lines_of_file (to_string file)
let read_lines_to_list file =
  FileString.read_lines_to_list (to_string file)
let read_lines_to_revlist file =
  FileString.read_lines_to_revlist (to_string file)

let read_subfile file pos len =
  FileString.read_subfile (to_string file) pos len
let string_of_subfile = read_subfile

let file_of_lines = write_lines
let lines_of_file = read_lines

let string_of_file = read_file
let file_of_string = write_file

let rename t1 t2 = Sys.rename (to_string t1) (to_string t2)

let exists file = Sys.file_exists (to_string file)
let is_directory filename =
  try let s = MinUnix.stat (to_string filename) in
    s.MinUnix.st_kind = MinUnix.S_DIR with _ -> false

let is_link file = FileString.is_link (to_string file)
let size file = FileString.size (to_string file)

let stat filename = MinUnix.stat (to_string filename)
let lstat filename = MinUnix.lstat (to_string filename)

let getcwd () = of_string (Sys.getcwd ())

  (*
    let size64 filename =
    let s = MinUnix.LargeFile.stat (to_string filename) in
    s.MinUnix.LargeFile.st_size
  *)

let open_in filename = open_in (to_string filename)
let open_out filename = open_out (to_string filename)

let open_in_bin filename = open_in_bin (to_string filename)
let open_out_bin filename = open_out_bin (to_string filename)


let copy_file f1 f2 =
  FileString.copy_file (to_string f1) (to_string f2)

let open_fd file mode perm = MinUnix.openfile (to_string file) mode perm

let remove file = Sys.remove (to_string file)

let iter_blocks f file =
  FileString.iter_blocks f (to_string file)

(*let safe_mkdir ?mode dir = FileString.safe_mkdir ?mode (to_string dir) *)
let copy_rec src dst = FileString.copy_rec (to_string src) (to_string dst)
let uncopy_rec src dst = FileString.uncopy_rec (to_string src) (to_string dst)


let extensions file = FileString.extensions_of_basename file.file_basename

let last_extension file =
  FileString.last_extension (basename file)

let chop_extension f =
  let (basename, _ext) = EzString.cut_at f.file_basename '.' in
  let ext_len = String.length f.file_basename - String.length basename in
  if ext_len = 0 then f else
    let len = String.length f.file_string in
    { f with
      file_basename = basename;
      file_string = String.sub f.file_string 0 (len-ext_len);
    }

let equal t1 t2 =
  t1.file_string = t2.file_string &&
  t1.file_partition = t2.file_partition


let temp_file t ext =
  of_string (Filename.temp_file (to_string t) ext)

let current_dir_name = of_string "."

let to_rooted_string t =
  if is_absolute t then
    t.file_string
  else
    Printf.sprintf ".%c%s" FileOS.dir_separator t.file_string

(*
module String = FileString
module Lines = FileLines
module Channel = FileChannel
module OS = FileOS
 *)

(*
let safe_basename s =
  basename (of_string s)
*)

module OP = struct

  let (//) t s = add_basenames t (EzString.split s '/')

end

module Directory_operations = FileDirMaker.Make(struct
    type path = t
    let to_string = to_string
    let add_basename = add_basename
    let dirname = dirname
    let basename = basename

    let rmdir s = MinUnix.rmdir (to_string s)
    let lstat s = MinUnix.lstat (to_string s)
    let stat s = MinUnix.stat (to_string s)
    let mkdir s perm = MinUnix.mkdir (to_string s) perm

    let remove s = Sys.remove (to_string s)
    let readdir s = Sys.readdir (to_string s)
  end)

include Directory_operations

let find_in_path path name =
  let file = of_string name in
  if not (is_implicit file) then
    if exists file then file
    else raise Not_found
  else
    let rec try_dir = function
        [] -> raise Not_found
      | dir::rem ->
        let dir = of_string dir in
        let fullname = concat dir file in
        if exists fullname then fullname else try_dir rem
    in
    try_dir path
