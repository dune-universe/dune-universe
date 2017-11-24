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

(** Context utilities. *)
open Wcs_t


(** {8 The ["skip_user_input"] field} *)

val skip_user_input_lbl : string
(** The ["skip_user_input"] string. *)

val skip_user_input : bool -> json
(** [skip_user_input b] creates the JSON object [{ "skip_user_input" : b }]. *)

val set_skip_user_input : json -> bool -> json
(**
   [set_skip_user_input ctx b] set the field ["skip_user_input"] of
   the object [ctx] with value [b].
*)

val take_skip_user_input : json -> json * bool
(**
   [take_skip_user_input ctx] take the field ["skip_user_input"] of
   the object [ctx]. If the field is the defined, it returns [false].
*)


(** {8 The ["actions"] field} *)

val actions_lbl : string
(** The ["actions"] string. *)

val actions : action list -> json
(** [actions acts] creates the JSON object [{ "actions" : acts }]. *)

val set_actions : json -> action list -> json
(**
   [set_actions ctx l] set the field ["actions"] of
   the object [ctx] with the list of actions [l].
*)

val take_actions : json -> json * action list option
(**
   [take_actions ctx] take the field ["actions"] of the object
   [ctx].
*)

val push_action : json -> action -> json
(**
   [push_action ctx act] add the action [act] in the list of actions
   stored in the field ["actions"] of ctx. It the field ["actions"]
   doesn't exists, it creates it.
*)

val pop_action : json -> json * action option
(**
   [pop_action ctx] take an action [act] in the list of actions
   stored in the field ["actions"] of ctx.
*)


(** {8 The ["continuation"] field} *)

val continuation_lbl : string
(** The ["continuation"] string. *)

val set_continuation : json -> action -> json
(**
   [set_continuation ctx act] set the field ["continuation"] of
   the object [ctx] with the action [act].
*)

val get_continuation : json -> action option
(**
   [get_continuation ctx] get the value of the field ["continuation"]
   of the object [ctx].
*)

val take_continuation : json -> json * action option
(**
   [take_continuation ctx] take the value of the field ["continuation"]
   of the object [ctx].
*)


(** {8 The ["return"] field} *)

val return_lbl : string
(** The ["return"] string. *)

val return : json -> json
(** [return v] creates the JSON object [{ "return" : v }]. *)

val set_return : json -> json -> json
(**
   [set_return ctx v] set the field ["return"] of
   the object [ctx] with the value [v].
*)

val get_return : json -> json option
(**
   [get_return ctx] get the value of the field ["return"]
   of the object [ctx].
*)
