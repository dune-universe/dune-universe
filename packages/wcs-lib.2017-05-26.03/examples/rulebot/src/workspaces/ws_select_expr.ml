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
open Cnl_util

let intents = []
let entities = [
  sys_number
]

let select_example =
  dialog_node "Select example"
    ~conditions: "@sys-number"
    ~next_step_id: ("expr1", Goto_condition)
    ()

let expr1 =
  dialog_node "expr1"
    ~parent: select_example
    ~conditions: "@sys-number == 1"
    ~text: "Here is the expression 1."
    ~context: (Ctx.set_expr `Null "expr" Cnl_samples.expr1)
    ()

let expr2 =
  dialog_node "expr2"
    ~parent: select_example
    ~conditions: "@sys-number == 2"
    ~text: "Here is the expression 2."
    ~context: (Ctx.set_expr `Null "expr" Cnl_samples.expr2)
    ()

let expr_unknown =
  dialog_node "expr unknown"
    ~parent: select_example
    ~text: "Sorry there is no example @sys-number."
    ()

let anything_else =
  dialog_node "Anything else"
    ~text: "Tell me which expression you would like."
    ~conditions: "anything_else"
    ()

let ws_select_expr =
  workspace "rulebot-select-expr"
    ~description: "Dialog to select an expression"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ select_example;
                     expr1;
                     expr2;
                     expr_unknown;
                     anything_else ]
    ()
