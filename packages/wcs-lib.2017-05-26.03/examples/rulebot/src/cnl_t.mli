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

open Wcs_lib

(* type 'a error_or = ('a, string) result *)

(** {6. Locations} *)

type location = Lexing.position * Lexing.position

(** {6. Nodes}*)

type id = int option

type 'a node =
  | N_undefined of id
  | N_filled of id * 'a
  | N_rejected of id * 'a
  | N_accepted of 'a

type 'a node_list = {
  list_elems : 'a list;
  list_closed : unit node;
}

(** {6. AST}
    See BNF in ../papers/2017-debs-dialog-odm/cnl_bnf.txt
*)

type cnl_rule = {
  rule_node : cnl_rule_desc node;
  rule_loc : location;
}
and cnl_rule_desc = {
  rule_evnt : cnl_event;
  rule_cond : cnl_cond;
  rule_actns : cnl_actions;
}

and cnl_event = {
  evnt_node : cnl_evnt_desc node;
  evnt_loc : location;
}
and cnl_evnt_desc = event_name * variable_name option

and cnl_cond = {
  cond_node : cnl_cond_desc node;
  cond_loc : location;
}
and cnl_cond_desc =
  | C_no_condition
  | C_condition of cnl_expr

and cnl_actions = {
  actns_node : cnl_actns_desc node;
  actns_loc : location;
}
and cnl_actns_desc = cnl_action node_list

and cnl_action = {
  actn_node : cnl_actn_desc node;
  actn_loc : location;
}
and cnl_actn_desc =
  | A_print of cnl_expr
  | A_emit of cnl_expr
  | A_define of variable_name * cnl_expr
  | A_set of field_name * variable_name * cnl_expr

and cnl_expr = {
  expr_node : cnl_expr_desc node;
  expr_field : (event_name * field_name) option; (* XXX Hack? -- contextual information for fields within new XXX *)
  expr_loc : location;
}
and cnl_expr_desc =
  | E_lit of cnl_literal
  | E_var of variable_name
  | E_get of cnl_expr * field_name
  | E_agg of cnl_aggop * cnl_expr * field_name (* XXX TODO: review *)
  | E_unop of cnl_unop * cnl_expr
  | E_binop of cnl_binop * cnl_expr * cnl_expr
  | E_error of Json_t.safe
  | E_this of string (* for current object *)
  | E_new of event_name * (cnl_setter list)
and cnl_setter = field_name * cnl_expr

and cnl_literal =
  | L_string of string
  | L_int of int
  | L_int_as_string of string
  | L_real of float
  | L_real_as_string of string
  | L_boolean of bool
  | L_boolean_as_string of string
  | L_enum of string
  | L_date of string
  | L_duration of string

and event_name = string
and variable_name = string
and field_name = string

and cnl_unop =
  | Op_not
  | Op_toString

and cnl_binop =
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
  | Op_plus
  | Op_minus
  | Op_mult
  | Op_div
  | Op_mod
  | Op_pow
  | Op_concat
  | Op_during

and cnl_aggop =
  | A_total
  | A_avg


(** {6. CNL}*)

type cnl_kind =
  | K_expr of (event_name * field_name) option
  | K_actn
  | K_evnt
  | K_cond
  | K_actns
  | K_actns_closed
  | K_rule

type cnl_ast =
  | Cnl_expr of cnl_expr
  | Cnl_actn of cnl_action
  | Cnl_evnt of cnl_event
  | Cnl_cond of cnl_cond
  | Cnl_actns of cnl_actions
  | Cnl_rule of cnl_rule


(** {6 JSON serialization} *)
val cnl_expr_desc_of_yojson : Json_t.safe -> (cnl_expr_desc, string) Deriving_intf.deriving_result
val cnl_expr_desc_to_yojson : cnl_expr_desc -> Json_t.safe

val cnl_expr_of_yojson : Json_t.safe -> (cnl_expr, string) Deriving_intf.deriving_result
val cnl_expr_to_yojson : cnl_expr -> Json_t.safe

val cnl_actn_desc_of_yojson : Json_t.safe -> (cnl_actn_desc, string) Deriving_intf.deriving_result
val cnl_actn_desc_to_yojson : cnl_actn_desc -> Json_t.safe

val cnl_action_of_yojson : Json_t.safe -> (cnl_action, string) Deriving_intf.deriving_result
val cnl_action_to_yojson : cnl_action -> Json_t.safe

val cnl_evnt_desc_of_yojson : Json_t.safe -> (cnl_evnt_desc, string) Deriving_intf.deriving_result
val cnl_evnt_desc_to_yojson : cnl_evnt_desc -> Json_t.safe

val cnl_event_of_yojson : Json_t.safe -> (cnl_event, string) Deriving_intf.deriving_result
val cnl_event_to_yojson : cnl_event -> Json_t.safe

val cnl_cond_desc_of_yojson : Json_t.safe -> (cnl_cond_desc, string) Deriving_intf.deriving_result
val cnl_cond_desc_to_yojson : cnl_cond_desc -> Json_t.safe

val cnl_cond_of_yojson : Json_t.safe -> (cnl_cond, string) Deriving_intf.deriving_result
val cnl_cond_to_yojson : cnl_cond -> Json_t.safe

val cnl_actns_desc_of_yojson : Json_t.safe -> (cnl_actns_desc, string) Deriving_intf.deriving_result
val cnl_actns_desc_to_yojson : cnl_actns_desc -> Json_t.safe

val cnl_actions_of_yojson : Json_t.safe -> (cnl_actions, string) Deriving_intf.deriving_result
val cnl_actions_to_yojson : cnl_actions -> Json_t.safe

val cnl_rule_desc_of_yojson : Json_t.safe -> (cnl_rule_desc, string) Deriving_intf.deriving_result
val cnl_rule_desc_to_yojson : cnl_rule_desc -> Json_t.safe

val cnl_rule_of_yojson : Json_t.safe -> (cnl_rule, string) Deriving_intf.deriving_result
val cnl_rule_to_yojson : cnl_rule -> Json_t.safe

