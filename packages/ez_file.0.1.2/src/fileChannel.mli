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

(** File operations on the stdlib [in_channel]/[out_channel] types *)

include (FileSig.CONTENT_OPERATIONS with
          type in_file := in_channel
          and type out_file = out_channel)

(* [output_line ic line] outputs [line] in [ic], followed by a
   line-terminator. *)
val output_line : out_channel -> string -> unit
