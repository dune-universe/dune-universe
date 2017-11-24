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

open Cnl_t

type workspace_ids = {
  ws_dispatch_id : string;
  ws_when_id : string;
  ws_cond_id : string;
  ws_cond_continue_id : string;
  ws_then_id : string;
  ws_expr_id : string;
  ws_actn_id : string;
  ws_accept_id : string;
}

type 'a dispatch = {
  dsp_replace : bool;
  dsp_abort : bool;
  dsp_number : 'a option;
  dsp_when : bool;
  dsp_cond : bool;
  dsp_then : bool;
}

val bypass_expr : string -> (bool * cnl_expr) option
val bypass_empty : string -> (bool * 'a) option

val match_string : string -> (string * string) option

val debug_message : Wcs_t.message_request -> Wcs_t.message_response -> unit

(** {6 JSON serialization} *)
val dispatch_of_yojson :
  (Json_t.safe -> 'a Deriving_intf.deriving_error_or) ->
  Json_t.safe -> ('a dispatch, string) Deriving_intf.deriving_result
val dispatch_to_yojson :
  ('a -> Json_t.safe) -> 'a dispatch -> Json_t.safe

val int_dispatch_of_yojson :
  Json_t.safe -> (int dispatch, string) Deriving_intf.deriving_result

val string_dispatch_to_yojson : string dispatch -> Json_t.safe
