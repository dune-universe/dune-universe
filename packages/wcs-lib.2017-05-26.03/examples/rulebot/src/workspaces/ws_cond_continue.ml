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
    ~description: "The ask for additional conditions"
    ~text: "Any other conditions?"
    ()

let present_condition =
  dialog_node "There is a condition"
    ~parent: create_condition
    ~conditions: "@yes"
    ~text: "Ok, there is an additional condition for this rule"
    ~context: (Ctx.set_bool
                 (Ctx.set_skip_user_input `Null true)
                 "more_cond" true)
    ()

let empty_condition =
  dialog_node "There is no condition"
    ~parent: create_condition
    ~conditions: "@no"
    ~text: "Ok no more condition"
    ~context: (Ctx.set_bool `Null "more_cond" false)
    ()

let response_handler =
  dialog_node "Do not understand"
    ~parent: create_condition
    ~conditions: "@no"
    ~text: "Sorry, I don't understand"
    ~next_step: (create_condition, Goto_body)
    ()

let ws_cond_continue =
  workspace "rulebot-cond-continue"
    ~description: "Dialog to add conditions to a rule condition"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ create_condition;
                     present_condition;
                     empty_condition;
                     response_handler;
                     Ws_common.anything_else; ]
    ()
