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

(** Pretty print Wcs data structures as JSON objects. *)

open Wcs_t

val workspace_response : workspace_response -> string
(** workspace_response *)

val pagination_response : pagination_response -> string
(** pagination_response *)

val list_workspaces_request : list_workspaces_request -> string
(** list_workspaces_request *)

val list_workspaces_response : list_workspaces_response -> string
(** list_workspaces_response *)

val intent_example : intent_example -> string
(** intent_example *)

val intent_def : intent_def -> string
(** intent_def *)

val entity_value : entity_value -> string
(** entity_value *)

val entity_def : entity_def -> string
(** entity_def *)

val next_step : next_step -> string
(** next_step *)

val output_def : output_def -> string
(** output_def *)

val dialog_node : dialog_node -> string
(** dialog_node *)

val workspace : workspace -> string
(** workspace *)

val input : input -> string
(** input *)

val entity : entity -> string
(** entity *)

val output : output -> string
(** output *)

val message_request : message_request -> string
(** message_request *)

val message_response : message_response -> string
(** message_response *)

val create_response : create_response -> string
(** create_response *)

val get_workspace_request : get_workspace_request -> string
(** get_workspace_request *)

val action : action -> string
(** action *)

val action_def : action_def -> string
(** action_def *)

val log_entry : log_entry -> string
(** log_entry *)

val logs_request : logs_request -> string
(** logs_request *)

val logs_response : logs_response -> string
(** logs_response *)
