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

open Wcs_t

(** Type of a condtional response *)
type response = {
  r_conditions : string option;
  r_output : json option;
  r_context : json option;
}

(** Type of a slot *)
type slot = {
  s_prompt : output_def;
  s_conditions : string;
  s_variable : string;
  s_context : json option;
  s_match : response list;
  s_nomatch : response list;
}

(** Type of dialog nodes. *)
type node = {
  n_dialog_node : string;
  n_description : string option;
  n_conditions : string option;
  n_prompt : output_def;
  n_reactions : response list;
  n_responses : response list;
  n_slots : slot list;
  n_metadata : json option;
  n_next_step : next_step option;
  n_created : string option;
  n_updated : string option;
}

(** Type of dialog trees. *)
type dialog =
  | D of (node * dialog) list
