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

(** File operations with filenames represented by an abstract type
 [FileGen.t] *)

(* This module implements operations on filenames, represented by an
   abstract type, so that manipulations of filenames are better checked. *)

include (FileSig.FILE_OPERATIONS)

val equal : t -> t -> bool

(* conversions to and from filenames *)
val to_string : t -> string
val of_string : string -> t

(* OS specific versions *)
val of_unix_string : string -> t
val of_win32_string : string -> t

(* Force either /absolute_path or ./implicit_path *)
val to_rooted_string : t -> string

val of_path : string -> string list -> t
