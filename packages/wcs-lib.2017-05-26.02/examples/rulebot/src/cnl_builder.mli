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

val mk_expr_with_loc :
  cnl_expr_desc node -> (string * string) option -> location -> cnl_expr
val mk_expr : cnl_expr_desc node -> cnl_expr
val mk_expr_undefined : unit -> cnl_expr
val mk_expr_f : cnl_expr_desc -> cnl_expr
val mk_expr_in_field : cnl_expr_desc node -> (string * string) -> cnl_expr
val mk_expr_in_field_undefined : (string * string) -> cnl_expr
val mk_expr_in_field_f : cnl_expr_desc -> (string * string) -> cnl_expr
val mk_boolean_f : bool -> cnl_expr
val mk_int_f : int -> cnl_expr
val mk_int_as_string_f : string -> cnl_expr
val mk_float_f : float -> cnl_expr
val mk_float_as_string_f : string -> cnl_expr
val mk_boolean_as_string_f : string -> cnl_expr
val mk_string_f : string -> cnl_expr
val mk_enum_f : string -> cnl_expr
val mk_var_f : variable_name -> cnl_expr
val mk_get_f : cnl_expr -> field_name -> cnl_expr
val mk_aggregate_f :
  cnl_expr -> cnl_aggop -> field_name -> cnl_expr
val mk_get_var_f : variable_name -> field_name -> cnl_expr
val mk_concat_f : cnl_expr -> cnl_expr -> cnl_expr
val mk_concat_list_f : cnl_expr list -> cnl_expr
val mk_lt_f : cnl_expr -> cnl_expr -> cnl_expr
val mk_plus_f : cnl_expr -> cnl_expr -> cnl_expr
val mk_div_f : cnl_expr -> cnl_expr -> cnl_expr
val mk_binop_expr_f :
  cnl_binop -> cnl_expr -> cnl_expr -> cnl_expr
val mk_binop_f : cnl_binop -> cnl_expr
val mk_unop_f : cnl_unop -> cnl_expr
val mk_this_f : string -> cnl_expr
val mk_new_event_f :
  event_name -> cnl_setter list -> cnl_expr
val mk_new_event_for_concept_f :
  string -> string list -> cnl_expr
val mk_avg_f : field_name -> cnl_expr -> cnl_expr
val mk_evnt_with_loc :
  cnl_evnt_desc node -> location -> cnl_event
val mk_evnt : cnl_evnt_desc node -> cnl_event
val mk_evnt_undefined : unit -> cnl_event
val mk_evnt_f : cnl_evnt_desc -> cnl_event
val mk_cond_with_loc :
  cnl_cond_desc node -> location -> cnl_cond
val mk_cond : cnl_cond_desc node -> cnl_cond
val mk_cond_undefined : unit -> cnl_cond
val mk_cond_f : cnl_cond_desc -> cnl_cond
val mk_actn_with_loc :
  cnl_actn_desc node -> location -> cnl_action
val mk_actn : cnl_actn_desc node -> cnl_action
val mk_actn_f : cnl_actn_desc -> cnl_action
val mk_actn_undefined : unit -> cnl_action
val mk_print_f : cnl_expr -> cnl_action
val mk_emit_f : cnl_expr -> cnl_action
val mk_define_f : variable_name -> cnl_expr -> cnl_action
val mk_set_desc_f :
  field_name ->
  variable_name -> cnl_expr -> cnl_actn_desc
val mk_set_f :
  field_name ->
  variable_name -> cnl_expr -> cnl_action
val mk_actns_with_loc :
  cnl_actns_desc node -> location -> cnl_actions
val mk_actns : cnl_actns_desc node -> cnl_actions
val mk_actns_undefined : unit -> cnl_actions
val mk_actns_f : cnl_actns_desc -> cnl_actions
val mk_actns_desc_undefined : unit -> cnl_actns_desc
val mk_rule_desc :
  cnl_event ->
  cnl_cond -> cnl_actions -> cnl_rule_desc
val mk_rule_f :
  cnl_event -> cnl_cond -> cnl_actions -> cnl_rule
val mk_rule_init :
  cnl_evnt_desc ->
  cnl_cond_desc -> cnl_actns_desc -> cnl_rule
val mk_rule_undefined : unit -> cnl_rule
