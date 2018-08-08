(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* File identity by device+inode or md5sum of contents 

   In Mingw, inode does not work. We use md5sum of the contents instead.
*)

open Utils

type t = 
  | Dev_inode of int * int
  | Md5sum of Digest.t

let get = Hashtbl.memoize (Hashtbl.create 107) (fun path ->
  let ident = 
    try
      let st = Unix.lstat path in
      if st.Unix.st_ino = 0 then (* Mingw *)
        Some (Md5sum (Digest.file path))
      else 
        Some (Dev_inode (st.Unix.st_dev, st.Unix.st_ino))
    with
    | _ -> None
  in
  path, ident)
  
