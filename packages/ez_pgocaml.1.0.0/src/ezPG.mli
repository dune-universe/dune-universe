(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2021 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val connect :
  ?host:string ->
  ?port:int ->
  ?user:string ->
  ?password:string ->
  ?unix_domain_socket_dir:string ->
  string -> 'a PGOCaml.t

val close : 'a PGOCaml.t -> unit

val exec :
  ?verbose:bool -> (* print commands, true by default *)
  'a PGOCaml.t -> (* database handler *)
  ?callback: (* function called with results, None = error *)
    (string list list option -> unit) ->
  string -> (* Query *)
  unit

val execs : (* same as exec, but with a list of queries *)
  ?verbose:bool ->
  'a PGOCaml.t ->
  string list ->
  unit

val upgrade_database :
  ?verbose:bool -> (* print commands, false by default *)
  ?downgrades: (int * string list) list ->
  ?allow_downgrade: bool ->
  upgrades: (* migration scripts *)
    (int * ('a PGOCaml.t -> int -> unit)) list ->
  ?target:int -> (* target version *)
  ?witness:string -> (* a file modified if the db is modified *)
  'a PGOCaml.t -> (* database handler *)
  unit

val touch_witness : ?witness:string -> int -> unit

(* ~searchpath can be used to register meta tables in a different
   domain (for example, "db") *)
val init : ?verbose:bool -> ?witness:string ->
  ?searchpath:string -> 'a PGOCaml.t -> unit

(* Useful functions to create the initial database *)
val createdb :
  ?verbose:bool ->
  ?host:string ->
  ?port:int ->
  ?unix_domain_socket_dir:string ->
  string -> unit
val dropdb :
  ?verbose:bool ->
  ?host:string ->
  ?port:int ->
  ?unix_domain_socket_dir:string ->
  string -> unit

val begin_tr : 'a PGOCaml.t -> unit
val end_tr : 'a PGOCaml.t -> unit
val abort_tr : 'a PGOCaml.t -> unit

val in_tr : 'a PGOCaml.t -> ('a PGOCaml.t -> unit) -> unit

val upgrade :
  ?verbose:bool -> version:int ->
  ?downgrade:string list ->
  dbh:'c PGOCaml.t -> string list -> unit

val printf :
  ?verbose:bool ->
  ?callback:(string list list option -> unit) ->
  'a PGOCaml.t -> ('b, unit, string, unit) format4 -> 'b

val may_upgrade_old_info : ?verbose:bool -> 'a PGOCaml.t -> unit

(* Add columns row_created_ and row_modified_ to a table,
   automatically updated in INSERT and UPDATE by a trigger.*)
module Mtimes : sig

  val upgrade_init : string list
  val downgrade_init : string list
  val upgrade_table : string -> string list
  val downgrade_table : string -> string list

  end
