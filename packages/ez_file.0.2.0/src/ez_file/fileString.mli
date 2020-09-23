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

(** File operations with filenames represented by the [string] type *)

(* This module implements operations on filenames represented as strings,
   as in the Filename module of the Standard Library. *)

include (FileSig.FILE_OPERATIONS with type t := string)
