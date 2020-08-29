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


(* obsolete !!! Do not use ! Use `FileSig.DIRECTORY_OPERATIONS` included in
   `FileGen` *)

open OcpCompat

val mkdir : FileGen.t -> int -> unit

(* mkdir, with potentially any non-existing parent directory *)
val safe_mkdir : ?mode:int -> FileGen.t -> unit

(* deprecated, use mkdir and mkdir_all *)
val make : FileGen.t -> unit
val make_all : FileGen.t -> unit

val list : FileGen.t -> string list
val list_files : FileGen.t -> FileGen.t list
val iter : (string -> unit) -> FileGen.t -> unit
val iter_files : (FileGen.t -> unit) -> FileGen.t -> unit
val remove : FileGen.t -> unit
val remove_all : FileGen.t -> unit
