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
open Wcs_t
open Wcs
open Cnl_t
open Cnl_builder
open Cnl_util

let intents = []

let entities =
  Ws_common.entity_yes ::
  Ws_common.entity_no ::
  Ws_common.bom_sample_entities

let create_condition =
  dialog_node "Create Rule"
    ~description: "The top of the create/rule flow"
    ~text: "Is there a condition?"
    ()

let present_condition =
  let instr = C_condition (mk_expr_undefined ()) in
  dialog_node "There is a condition"
    ~parent: create_condition
    ~conditions: "@yes"
    ~text: "Ok, there is a condition for this rule"
    ~context: (Ctx.set_cond_desc
                 (Ctx.set_skip_user_input `Null true)
                 "cond_desc" instr)
    ()

let empty_condition =
  let instr = C_no_condition in
  dialog_node "There is no condition"
    ~parent: create_condition
    ~conditions: "@no"
    ~text: "Ok the condition for this rule is empty"
    ~context: (Ctx.set_cond_desc `Null "cond_desc" instr)
    ()

let response_handler =
  dialog_node "Do not understand"
    ~parent: create_condition
    ~conditions: "@no"
    ~text: "Sorry, I don't understand"
    ~next_step: (create_condition, Goto_body)
    ()

let ws_cond =
  workspace "rulebot-cond"
    ~description: "Dialog to build the condition part of a rule"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ create_condition;
                     present_condition;
                     empty_condition;
                     response_handler;
                     Ws_common.anything_else; ]
    ()
