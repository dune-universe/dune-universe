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
open Cnl_t
open Call_t

val null : json

val set : json -> string -> json -> json
val take : json -> string -> json * json option
val get : json -> string -> json option

val set_skip_user_input : json -> bool -> json
val take_skip_user_input : json -> json * bool

val set_call : json -> call -> json
val take_call : json -> json * call option

val set_return : json -> json -> json
val get_return : json -> json option

val set_rule : json -> string -> cnl_rule -> json
val get_rule : json -> string -> cnl_rule option

val set_expr : json -> string -> cnl_expr -> json
val get_expr : json -> string -> cnl_expr option

val set_evnt_desc : json -> string -> cnl_evnt_desc -> json
val get_evnt_desc : json -> string -> cnl_evnt_desc option

val set_cond_desc : json -> string -> cnl_cond_desc -> json
val get_cond_desc : json -> string -> cnl_cond_desc option

val set_actns_desc : json -> string -> cnl_actns_desc -> json
val get_actns_desc : json -> string -> cnl_actns_desc option

val set_actn_desc : json -> string -> cnl_actn_desc -> json
val get_actn_desc : json -> string -> cnl_actn_desc option

val set_bool : json -> string -> bool -> json
val get_bool : json -> string -> bool option

val set_string : json -> string -> string -> json
val get_string : json -> string -> string option
val take_string : json -> string -> json * string option

val set_dispatch : json -> string -> string Dialog_util.dispatch -> json
val get_dispatch : json -> string -> int Dialog_util.dispatch option

val build_cnl : cnl_kind -> int -> string -> json
