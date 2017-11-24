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

open Wcs

let entities = [
  Ws_common.entity_yes;
  Ws_common.entity_no
]

let intents = []

let question =
  dialog_node "Question"
    ~text: "Do you accept the $kind $n?"
    ()

let yes =
  dialog_node "Yes"
    ~parent: question
    ~text: "The $kind is confirmed."
    ~context: (`Assoc ["accept", `Bool true])
    ~conditions: "@yes"
    ()

let no =
  dialog_node "No"
    ~parent: question
    ~conditions: "@no"
    ~text: "The $kind is rejected."
    ~context: (`Assoc ["accept", `Bool false])
    ()

let ws_accept =
  workspace "rulebot-accept"
    ~description: "Dialog to get confirmation from the user"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ question; yes; no; Ws_common.anything_else; ]
    ()
