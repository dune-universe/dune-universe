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

(** {6. Printer util} *)

val string_of_cnl_agg : cnl_aggop -> string

val string_of_cnl_unop : cnl_unop -> string

val string_of_cnl_binop : cnl_binop -> string

(** {6. Access} *)

val node_id : 'a node -> id option

val node_desc : 'a node -> 'a option


val rule_find_node_kind : id -> cnl_rule -> cnl_kind option

(** {6. Collect Nodes} *)

(** {8. Undefined} *)

val rule_get_undefined : cnl_rule -> (id * cnl_kind) list

val evnt_get_undefined : cnl_event -> (id * cnl_kind) list

val cond_get_undefined : cnl_cond -> (id * cnl_kind) list

val actns_get_undefined :
  cnl_actions -> (id * cnl_kind) list

val actn_get_undefined : cnl_action -> (id * cnl_kind) list

val expr_get_undefined : cnl_expr -> (id * cnl_kind) list


(** {8. Filled} *)

val rule_get_filled : cnl_rule -> (id * cnl_kind) list

val evnt_get_filled : cnl_event -> (id * cnl_kind) list

val cond_get_filled : cnl_cond -> (id * cnl_kind) list

val actns_get_filled : cnl_actions -> (id * cnl_kind) list

val actn_get_filled : cnl_action -> (id * cnl_kind) list

val expr_get_filled : cnl_expr -> (id * cnl_kind) list


(** {8. Rejected} *)

val rule_get_rejected : cnl_rule -> (id * cnl_kind) list

val evnt_get_rejected : cnl_event -> (id * cnl_kind) list

val cond_get_rejected : cnl_cond -> (id * cnl_kind) list

val actns_get_rejected :
  cnl_actions -> (id * cnl_kind) list

val actn_get_rejected : cnl_action -> (id * cnl_kind) list

val expr_get_rejected : cnl_expr -> (id * cnl_kind) list


(** {6. Find focus} *)

val rule_next_focus : int -> cnl_rule -> (int * cnl_kind) option

val cond_next_focus : int -> cnl_rule -> (int * cnl_kind) option


(** {6. Get subtree} *)

val expr_get_cnl : id -> cnl_expr -> cnl_ast option

val evnt_get_cnl : id -> cnl_event -> cnl_ast option

val cond_get_cnl : id -> cnl_cond -> cnl_ast option

val actn_get_cnl : id -> cnl_action -> cnl_ast option

val actns_get_cnl : id -> cnl_actions -> cnl_ast option

val rule_get_cnl : id -> cnl_rule -> cnl_ast option


(** {6. Renaming } *)

val index_rule : cnl_rule -> cnl_rule

(** {6. Change Node State} *)

(** {8. Filled to Accepted} *)

val f_to_a : 'a node -> 'a node

val expr_f_to_a : cnl_expr -> cnl_expr

val actn_f_to_a : cnl_action -> cnl_action

val evnt_f_to_a : cnl_event -> cnl_event

val cond_f_to_a : cnl_cond -> cnl_cond

val actns_f_to_a : cnl_actions -> cnl_actions

val rule_f_to_a : cnl_rule -> cnl_rule

val cnl_f_to_a : cnl_ast -> cnl_ast

(** {8. Filled to Reject} *)

val f_to_r : 'a node -> 'a node

val expr_f_to_r : cnl_expr -> cnl_expr

val actn_f_to_r : cnl_action -> cnl_action

val evnt_f_to_r : cnl_event -> cnl_event

val cond_f_to_r : cnl_cond -> cnl_cond

val actns_f_to_r : cnl_actions -> cnl_actions

val rule_f_to_r : cnl_rule -> cnl_rule

val cnl_f_to_r : cnl_ast -> cnl_ast

(** {6. modify tree } *)

val add_cond : cnl_rule -> cnl_rule

(** {6. prompt message} *)

val expr_prompt : id -> cnl_expr -> string
val rule_prompt : id -> cnl_rule -> string option


(** {6. Conversion to Yojson.Basic} *)

val json_of_expr : cnl_expr -> Json_t.basic
val json_of_expr_desc : cnl_expr_desc -> Json_t.basic
val json_of_evnt : cnl_event -> Json_t.basic
val json_of_evnt_desc : cnl_evnt_desc -> Json_t.basic
val json_of_cond : cnl_cond -> Json_t.basic
val json_of_cond_desc : cnl_cond_desc -> Json_t.basic
val json_of_actn : cnl_action -> Json_t.basic
val json_of_actn_desc : cnl_actn_desc -> Json_t.basic
val json_of_actns : cnl_actions -> Json_t.basic
val json_of_actns_desc : cnl_actns_desc -> Json_t.basic
val json_of_rule : cnl_rule -> Json_t.basic
val json_of_rule_desc : cnl_rule_desc -> Json_t.basic
val json_replace : string -> string -> Json_t.basic -> Json_t.basic
