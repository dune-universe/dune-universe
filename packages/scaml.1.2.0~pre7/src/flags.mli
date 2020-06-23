(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot

type mode =
  | Compile
  | ConvertAll
  | ConvertSingleValue of string
  | ConvertSingleType of string
  | Revert of string

and t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  ; scaml_debug : bool
  ; scaml_time : bool
  ; scaml_mode : mode option
  ; scaml_noscamlib : bool (** do not add -I `opam config var prefix`/scaml/lib *)
  ; dump_iml : bool
  } [@@deriving conv{ocaml}]

val flags : t ref

val pp : Format.t -> t -> unit
val eval : t -> Ppxlib.Longident.t * [`Bool of bool | `Constant of Ppxlib.Parsetree.constant ] -> (t, string) Result.t
val update : (t -> t) -> unit
val with_flags : (t -> t) -> (unit -> 'a) -> 'a
val if_debug : (unit -> unit) -> unit
val if_time : (unit -> unit) -> unit
val set_mode : t -> mode -> t
