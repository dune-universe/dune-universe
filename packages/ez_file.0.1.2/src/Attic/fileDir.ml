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

let remove_all filename = FileGen.remove_dir ~all:true filename
let remove filename = FileGen.remove_dir filename

let iter_files f filename =
  FileGen.iter_dir (fun _basename _path file -> f file) filename

let iter f filename =
  FileGen.iter_dir (fun basename _path _file -> f basename) filename

let list_files filename = FileGen.read_dir_to_list filename
let list filename = Array.to_list (FileGen.readdir filename)

let mkdir = FileGen.mkdir
let make filename = mkdir filename 0o755
let safe_mkdir ?mode filename = FileGen.make_dir ?mode ~p:true filename
let make_all = safe_mkdir ~mode:0o755
