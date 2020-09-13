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
  ; debug     : bool
  ; time      : bool
  ; mode      : mode
  ; noscamlib : bool (** do not add -I `opam config var prefix`/scaml/lib *)
  ; dump_iml  : bool
  ; protocol  : int * int (* 007, 000 *)
  } [@@deriving conv{ocaml}]


val pp : Format.t -> t -> unit

val get_conf : unit -> t
val if_debug : (unit -> unit) -> unit
val if_time : (unit -> unit) -> unit
val get_protocol : unit -> int * int

type opt =
  { op_iml_optimization : bool option
  ; op_debug     : bool option
  ; op_time      : bool option
  ; op_mode      : mode option
  ; op_noscamlib : bool option
  ; op_dump_iml  : bool option
  ; op_protocol  : Protocol.t option
  } [@@deriving conv{ocaml}]

val pp_opt : Format.t -> opt -> unit

val none : opt
val merge : opt -> opt -> opt (* may fail *)
val unopt : opt -> t
val eval
  : Untyped.Longident.t
    * [> `Bool of bool | `Constant of Untyped.Parsetree.constant ]
  -> (opt, string) result
val with_scaml_attrs
  : (Untyped.Longident.t Location.loc
     * [> `Bool of bool | `Constant of Untyped.Parsetree.constant ]) list
   -> (unit -> 'a) -> 'a
val get_opt : unit -> opt
val with_opt : opt -> (unit -> 'a) -> 'a
