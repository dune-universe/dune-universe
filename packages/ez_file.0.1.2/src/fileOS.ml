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

let win32 = (Sys.os_type = "Win32")
let dir_separator = if win32 then '\\' else '/'
let path_separator = if win32 then ';' else ':'
let dir_separator_string = String.make 1 dir_separator
let line_separator = if win32 then "\r\n" else "\n"

let default_buffer_size = if win32 then 32768 else 65536
