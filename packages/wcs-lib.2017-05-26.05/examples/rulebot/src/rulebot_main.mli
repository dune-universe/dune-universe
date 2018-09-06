(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Wcs_lib
open Cnl_t

type mode =
  | M_nothing
  | M_bot
  | M_ws_gen
  | M_ws_delete

val rulebot_mode : mode ref
val wcs_credential : string option ref
val workspaces_config : Dialog_interface_t.config option ref
val ws_update : bool ref
val is_slack : bool ref
val slackbot : string ref
val bom_io : string option ref
val load_io : string -> Io_t.io

val load_ws_ids : Wcs_t.credential ->
  Dialog_interface_t.config option ->
  bool -> string * Bmd_t.bmd_schema -> Dialog_util.workspace_ids

val bmd : string option ref
val args : (Arg.key * Arg.spec * Arg.doc) list
val anon_args : string -> unit
val usage : string

val workspaces_generation : string * Bmd_t.bmd_schema -> unit
val workspaces_delete : Wcs_t.credential -> unit
