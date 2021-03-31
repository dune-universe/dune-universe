(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2021 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val main :
  ?downgrades: (int * string list) list ->
  upgrades:(int * (unit PGOCaml.t -> int -> unit)) list ->
  string ->
  unit
