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

(** {6 workspace_response} *)

let json_of_workspace_response rsp =
  Yojson.Basic.from_string (Wcs_j.string_of_workspace_response rsp)

let pretty_workspace_response rsp =
  Yojson.Basic.pretty_to_string (json_of_workspace_response rsp)


(** {6 pagination_response} *)

let json_of_pagination_response rsp =
  Yojson.Basic.from_string (Wcs_j.string_of_pagination_response rsp)

let pretty_pagination_response rsp =
  Yojson.Basic.pretty_to_string (json_of_pagination_response rsp)


(** {6 list_workspaces_request} *)

let json_of_list_workspaces_request req =
  Yojson.Basic.from_string (Wcs_j.string_of_list_workspaces_request req)

let pretty_list_workspaces_request req =
  Yojson.Basic.pretty_to_string (json_of_list_workspaces_request req)


(** {6 list_workspaces_response} *)

let json_of_list_workspaces_response rsp =
  Yojson.Basic.from_string (Wcs_j.string_of_list_workspaces_response rsp)

let pretty_list_workspaces_response rsp =
  Yojson.Basic.pretty_to_string (json_of_list_workspaces_response rsp)


(** {6 intent_example} *)

let json_of_intent_example x =
  Yojson.Basic.from_string (Wcs_j.string_of_intent_example x)

let pretty_intent_example x =
  Yojson.Basic.pretty_to_string (json_of_intent_example x)


(** {6 intent_def} *)

let json_of_intent_def x =
  Yojson.Basic.from_string (Wcs_j.string_of_intent_def x)

let pretty_intent_def x =
  Yojson.Basic.pretty_to_string (json_of_intent_def x)


(** {6 entity_value} *)

let json_of_entity_value x =
  Yojson.Basic.from_string (Wcs_j.string_of_entity_value x)

let pretty_entity_value x =
  Yojson.Basic.pretty_to_string (json_of_entity_value x)


(** {6 entity_def} *)

let json_of_entity_def x =
  Yojson.Basic.from_string (Wcs_j.string_of_entity_def x)

let pretty_entity_def x =
  Yojson.Basic.pretty_to_string (json_of_entity_def x)


(** {6 next_step} *)

let json_of_next_step x =
  Yojson.Basic.from_string (Wcs_j.string_of_next_step x)

let pretty_next_step x =
  Yojson.Basic.pretty_to_string (json_of_next_step x)


(** {6 output_def} *)

let json_of_output_def x =
  Yojson.Basic.from_string (Wcs_j.string_of_output_def x)

let pretty_output_def x =
  Yojson.Basic.pretty_to_string (json_of_output_def x)


(** {6 dialog_node} *)

let json_of_dialog_node x =
  Yojson.Basic.from_string (Wcs_j.string_of_dialog_node x)

let pretty_dialog_node x =
  Yojson.Basic.pretty_to_string (json_of_dialog_node x)


(** {6 workspace} *)

let json_of_workspace x =
  Yojson.Basic.from_string (Wcs_j.string_of_workspace x)

let pretty_workspace x =
  Yojson.Basic.pretty_to_string (json_of_workspace x)


(** {6 input} *)

let json_of_input x =
  Yojson.Basic.from_string (Wcs_j.string_of_input x)

let pretty_input x =
  Yojson.Basic.pretty_to_string (json_of_input x)


(** {6 entity} *)

let json_of_entity x =
  Yojson.Basic.from_string (Wcs_j.string_of_entity x)

let pretty_entity x =
  Yojson.Basic.pretty_to_string (json_of_entity x)


(** {6 output} *)

let json_of_output x =
  Yojson.Basic.from_string (Wcs_j.string_of_output x)

let pretty_output x =
  Yojson.Basic.pretty_to_string (json_of_output x)


(** {6 message_request} *)

let json_of_message_request x =
  Yojson.Basic.from_string (Wcs_j.string_of_message_request x)

let pretty_message_request x =
  Yojson.Basic.pretty_to_string (json_of_message_request x)


(** {6 message_response} *)

let json_of_message_response x =
  Yojson.Basic.from_string (Wcs_j.string_of_message_response x)

let pretty_message_response x =
  Yojson.Basic.pretty_to_string (json_of_message_response x)


(** {6 create_response} *)

let json_of_create_response x =
  Yojson.Basic.from_string (Wcs_j.string_of_create_response x)

let pretty_create_response x =
  Yojson.Basic.pretty_to_string (json_of_create_response x)

(** {6 get_workspace_request} *)

let json_of_get_workspace_request x =
  Yojson.Basic.from_string (Wcs_j.string_of_get_workspace_request x)

let pretty_get_workspace_request x =
  Yojson.Basic.pretty_to_string (json_of_get_workspace_request x)



(* (\** {6 action} *\) *)

(* let json_of_action x = *)
(*   Yojson.Basic.from_string (Wcs_j.string_of_action x) *)

(* let pretty_action x = *)
(*   Yojson.Basic.pretty_to_string (json_of_action x) *)

(** {6 log_entry} *)

let json_of_log_entry x =
  Yojson.Basic.from_string (Wcs_j.string_of_log_entry x)

let pretty_log_entry x =
  Yojson.Basic.pretty_to_string (json_of_log_entry x)


(** {6 logs_request} *)

let json_of_logs_request x =
  Yojson.Basic.from_string (Wcs_j.string_of_logs_request x)

let pretty_logs_request x =
  Yojson.Basic.pretty_to_string (json_of_logs_request x)


(** {6 logs_response} *)

let json_of_logs_response x =
  Yojson.Basic.from_string (Wcs_j.string_of_logs_response x)

let pretty_logs_response x =
  Yojson.Basic.pretty_to_string (json_of_logs_response x)

