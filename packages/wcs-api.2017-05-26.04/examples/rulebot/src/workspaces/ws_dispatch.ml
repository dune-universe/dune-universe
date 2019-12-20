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

let entities = [
  entity "rule_part"
    ~values:[ "cond", [ "condition"; "if" ];
              "then", [ "actions"; "action" ];
              "when", [ "event" ] ]
    ();
  sys_number;
  Ws_common.entity_yes;
  Ws_common.entity_no;
  Ws_common.entity_bmd;
]

let intents = [
  intent "replace"
    ~examples: [ "replace";
                 "I would like to change";
                 "I would like to update";
                 "I would like to replace";
                 "change my mind";
                 "I don't want to do that"; ]
    ();
  intent "fill"
    ~examples: [ "fill";
                 "I would like to set";
                 "I would like to fill";
                 "I want to work on something else"; ]
    ();
  intent "reject"
    ~examples: [ "reject";
                 "It is not what I want";
                 "no";
                 "I would like to correct";
                 "I don't want that";
                 "You miss understoud"; ]
    ();
  intent "abort"
    ~examples: [ "abort";
                 "quit";
                 "reset";
                 "I want to stop"; ]
    ();
  Ws_common.intent_help;
]

let fill_hole =
  let dsp =
    { Dialog_util.dsp_abort = false;
      dsp_replace = true;
      dsp_number = Some "@sys-number";
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Fill Hole"
    ~conditions: "(#fill || #replace) && intents[0].confidence > 0.9 && @sys-number"
    ~text: "Ok, let's try to fill hole number @sys-number."
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let fill_when =
  let dsp =
    { Dialog_util.dsp_replace = true;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = true;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Fill when"
    ~conditions: "(#fill || #replace) && @rule_part:when"
    ~text: "Let's fill the when part of the rule."
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let fill_cond =
  let dsp =
    { Dialog_util.dsp_replace = true;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = true;
      dsp_then = false; }
  in
  dialog_node "Fill cond"
    ~conditions: "(#fill || #replace) && @rule_part:cond"
    ~text: "Sounds good"
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let fill_then =
  let dsp =
    { Dialog_util.dsp_replace = true;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = true; }
  in
  dialog_node "Fill then"
    ~conditions: "(#fill || #replace) && @rule_part:then"
    ~text: "Let's go fill the action part of the rule."
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let fill_previous =
  let dsp =
    { Dialog_util.dsp_abort = false;
      dsp_replace = true;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Fill previous"
    ~text: "Ok, let's try to change previous hole."
    ~conditions: "(#fill || #replace) && intents[0].confidence > 0.9"
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let reset =
  dialog_node "Reset"
    ~conditions: "#abort && intents[0].confidence > 0.9"
    ~text: "Are you sure that you want to reset?"
    ()

let reset_yes =
  let dsp =
    { Dialog_util.dsp_replace = false;
      dsp_abort = true;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Reset yes"
    ~parent: reset
    ~conditions: "@yes"
    ~text: "Ok let's reset!"
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let reset_no =
  let dsp =
    { Dialog_util.dsp_replace = false;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Reset no"
    ~parent: reset
    ~conditions: "@no"
    ~text: "Ok let's continue"
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let reset_handler =
  dialog_node "Reset handler"
    ~parent: reset
    ~conditions: "anything_else"
    ~text: "Sorry I don't understand?"
    ~next_step: (reset, Goto_body)
    ()


let help =
  let dsp =
    { Dialog_util.dsp_replace = false;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Help"
    ~conditions: "#mk_help && intents[0].confidence > 0.9"
    ~text: ("You can edit any part of the rule by asking 'change' and giving the number of the part to modify.\n"^
            "You can also ask 'change when', 'change if', or 'change then'\n"^
            "You can restart with a new rule with 'reset'.")
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()

let help_bmd bmd =
  let dsp =
    { Dialog_util.dsp_replace = false;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Help BMD"
    ~conditions: "#mk_help && intents[0].confidence > 0.9 && @bmd"
    ~text: ("The current BMD is " ^ "```" ^ bmd ^ "```")
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()


let anything_else =
  let dsp =
    { Dialog_util.dsp_replace = false;
      dsp_abort = false;
      dsp_number = None;
      dsp_when = false;
      dsp_cond = false;
      dsp_then = false; }
  in
  dialog_node "Anything else"
    ~conditions: "anything_else"
    ~context: (Ctx.set_dispatch
                 (Ctx.set_skip_user_input `Null true)
                 "dispatch" dsp)
    ()


let ws_dispatch bmd =
  workspace "rulebot-dispatch"
    ~description: "Dialog to dispatch to the appropriate part of the rule"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: [ fill_hole;
                     fill_when;
                     fill_cond;
                     fill_then;
                     fill_previous;
                     reset;
                     reset_yes;
                     reset_no;
                     reset_handler;
                     help_bmd bmd;
                     help;
                     anything_else; ]
    ()
