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

val mkdir : string -> int -> unit
val make : string -> unit
val make_all : string -> unit
val list : string -> string list
val list_files : string -> string list
val iter : (string -> unit) -> string -> unit
val iter_files : (string -> unit) -> string -> unit
val remove : string -> unit
val remove_all : string -> unit
