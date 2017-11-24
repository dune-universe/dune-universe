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

val set_rule_init : cnl_rule -> unit
val rule_init : unit -> cnl_rule
val empty_init : unit -> cnl_rule
val cond_init : unit -> cnl_cond_desc
val actns_init : unit -> cnl_actns_desc
val print_init : unit -> cnl_actn_desc
val emit_init : unit -> cnl_actn_desc
val define_init : variable_name -> cnl_actn_desc
val set_init : field_name -> variable_name -> cnl_actn_desc
val define1 : cnl_action
val emit1 : cnl_action
val when1 : string * string option
val cond1 : cnl_cond_desc
val then1 : cnl_action node_list
val rule1 : cnl_rule
val define21 : cnl_action
val define22 : cnl_action
val setdesc21 : cnl_actn_desc
val setdesc22 : cnl_actn_desc
val set21 : cnl_action
val set22 : cnl_action
val rule2 : cnl_rule
val cnl_samples : (string * cnl_rule) list
val expr1 : cnl_expr
val expr2 : cnl_expr
