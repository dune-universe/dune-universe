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

(** File identity by device+inode or md5sum of contents 

   In Mingw, inode does not work. We use md5sum of the contents instead.
*)

type t = 
  | Dev_inode of int * int 
  | Md5sum of Digest.t

val get : string -> string * t option
(** [get path] returns its file identification with cons-hashed path *)
