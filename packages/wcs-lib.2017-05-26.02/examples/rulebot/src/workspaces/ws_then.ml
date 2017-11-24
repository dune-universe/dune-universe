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
open Wcs
open Cnl_builder
open Cnl_util

let intents = []
let entities =
  Ws_common.entity_yes ::
  Ws_common.entity_no ::
  []

(* let create_rule = *)
(*   let instr = mk_actns_desc_undefined () in *)
(*   dialog_node "Create Rule" *)
(*     ~description: "The top of the create/rule flow" *)
(*     ~text: "Let's build the then part." *)
(*     ~context: (Ctx.set_actns_desc `Null "actns_desc" instr) *)
(*     () *)

let prompt =
  dialog_node "prompt"
    ~text: "Any other action?"
    ()

let add_element =
  dialog_node "Add element"
    ~conditions: "@yes"
    ~parent: prompt
    ~context: (Ctx.set_bool
                 (Ctx.set_skip_user_input `Null true)
                 "actns_closed" false)
    ()

let do_not_add_element =
  dialog_node "Do not add element"
    ~conditions: "@no"
    ~parent: prompt
    ~context: (Ctx.set_bool `Null "actns_closed" true)
    ()

let handler =
  dialog_node "Do not understand"
    ~parent: prompt
    ~text: "I don't understand."
    ~next_step: (prompt, Goto_body)
    ()


let ws_then =
  workspace "rulebot-then"
    ~description: "Dialog to build the action part of a rule"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ prompt;
                     add_element;
                     do_not_add_element;
                     handler;
                     Ws_common.anything_else ]
    ()
