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

(* This is supposed to be a generic interface for reading/writing files.
  Normally, we would have the same module for Python syntax.
  Unfortunately:
  * we have not yet completely abstracted away the syntax from SimpleConfig,
    there are still some OCaml-like specific stuff (like comments)
  * it is not clear if we can split printing in the same way, between
    beginner and advanced options, in the Python syntax, as final options
    might be added to the last defined section instead of the DEFAULT section.
*)


val parse : FileAbstract.t -> in_channel -> SimpleConfigTypes.option_module

val reset : unit -> unit
val save_module :
  with_help: bool ->
  indent: string ->
  Buffer.t ->
  (string list * string * SimpleConfigTypes.option_value) list ->
  unit

val save_value :
  indent: string -> Buffer.t -> SimpleConfigTypes.option_value -> unit

val save_binding :
  Buffer.t -> string -> SimpleConfigTypes.option_value -> unit
