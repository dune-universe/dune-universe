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

type cnl_instr =
  | I_repl_expr of int * cnl_expr_desc (* Replace at focus *)
  | I_repl_actn of int * cnl_actn_desc
  | I_repl_evnt of int * cnl_evnt_desc
  | I_repl_cond of int * cnl_cond_desc
  | I_repl_actns of int * cnl_actns_desc
  | I_repl_actns_closed of int * bool
  | I_conf_expr of int * bool          (* Confirm at focus *)
  | I_conf_actn of int * bool
  | I_conf_evnt of int * bool
  | I_conf_cond of int * bool
  | I_conf_actns of int * bool
  | I_conf_rule of int * bool
  | I_insr_actn                   (* Insert action *)

type cnl_program = cnl_instr list

val focus_of_instr : cnl_instr -> int

(* JSON export/import *)
val cnl_instr_of_yojson : Json_t.safe -> (cnl_instr, string) Deriving_intf.deriving_result
val cnl_instr_to_yojson : cnl_instr -> Json_t.safe

val cnl_program_of_yojson : Json_t.safe -> (cnl_program, string) Deriving_intf.deriving_result
val cnl_program_to_yojson : cnl_program -> Json_t.safe

