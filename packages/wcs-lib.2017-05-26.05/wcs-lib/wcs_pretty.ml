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

let workspace_response rsp =
  Yojson.Basic.pretty_to_string (Wcs.json_of_workspace_response rsp)

let pagination_response rsp =
  Yojson.Basic.pretty_to_string (Wcs.json_of_pagination_response rsp)

let list_workspaces_request req =
  Yojson.Basic.pretty_to_string (Wcs.json_of_list_workspaces_request req)

let list_workspaces_response rsp =
  Yojson.Basic.pretty_to_string (Wcs.json_of_list_workspaces_response rsp)

let intent_example x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_intent_example x)

let intent_def x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_intent_def x)

let entity_value x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_entity_value x)

let entity_def x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_entity_def x)

let next_step x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_next_step x)

let output_def x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_output_def x)

let dialog_node x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_dialog_node x)

let workspace x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_workspace x)

let input x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_input x)

let entity x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_entity x)

let output x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_output x)

let message_request x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_message_request x)

let message_response x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_message_response x)

let create_response x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_create_response x)

let get_workspace_request x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_get_workspace_request x)

let log_entry x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_log_entry x)

let action x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_action x)

let action_def x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_action_def x)

let logs_request x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_logs_request x)

let logs_response x =
  Yojson.Basic.pretty_to_string (Wcs.json_of_logs_response x)
