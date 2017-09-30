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

(** Conversion of Wcs data structures to JSON. *)

open Wcs_t

(** {6 workspace_response} *)

val json_of_workspace_response : workspace_response -> json

val pretty_workspace_response : workspace_response -> string


(** {6 pagination_response} *)

val json_of_pagination_response : pagination_response -> json

val pretty_pagination_response : pagination_response -> string


(** {6 list_workspaces_request} *)

val json_of_list_workspaces_request : list_workspaces_request -> json

val pretty_list_workspaces_request : list_workspaces_request -> string


(** {6 list_workspaces_response} *)

val json_of_list_workspaces_response : list_workspaces_response -> json

val pretty_list_workspaces_response : list_workspaces_response -> string


(** {6 intent_example} *)

val json_of_intent_example : intent_example -> json

val pretty_intent_example : intent_example -> string


(** {6 intent_def} *)

val json_of_intent_def : intent_def -> json

val pretty_intent_def : intent_def -> string


(** {6 entity_value} *)

val json_of_entity_value : entity_value -> json

val pretty_entity_value : entity_value -> string


(** {6 entity_def} *)

val json_of_entity_def : entity_def -> json

val pretty_entity_def : entity_def -> string


(** {6 next_step} *)

val json_of_next_step : next_step -> json

val pretty_next_step : next_step -> string


(** {6 output_def} *)

val json_of_output_def : output_def -> json

val pretty_output_def : output_def -> string


(** {6 dialog_node} *)

val json_of_dialog_node : dialog_node -> json

val pretty_dialog_node : dialog_node -> string


(** {6 workspace} *)

val json_of_workspace : workspace -> json

val pretty_workspace : workspace -> string


(** {6 input} *)

val json_of_input : input -> json

val pretty_input : input -> string


(** {6 entity} *)

val json_of_entity : entity -> json

val pretty_entity : entity -> string


(** {6 output} *)

val json_of_output : output -> json

val pretty_output : output -> string


(** {6 message_request} *)

val json_of_message_request : message_request -> json

val pretty_message_request : message_request -> string


(** {6 message_response} *)

val json_of_message_response : message_response -> json

val pretty_message_response : message_response -> string


(** {6 create_response} *)

val json_of_create_response : create_response -> json

val pretty_create_response : create_response -> string


(** {6 get_workspace_request} *)

val json_of_get_workspace_request : get_workspace_request -> json

val pretty_get_workspace_request : get_workspace_request -> string


(** {6 log_entry} *)

val json_of_log_entry : log_entry -> json

val pretty_log_entry : log_entry -> string

(** {6 logs_request} *)

val json_of_logs_request : logs_request -> json

val pretty_logs_request : logs_request -> string

(** {6 logs_response} *)

val json_of_logs_response : logs_response -> json

val pretty_logs_response : logs_response -> string
