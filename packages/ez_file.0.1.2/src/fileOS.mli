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

(** Operating-System specific values *)

(* are we running on Win32 ? *)
val win32 : bool

(* OS specific values *)
val path_separator : char
val dir_separator : char
val dir_separator_string : string
val line_separator : string

val default_buffer_size : int
