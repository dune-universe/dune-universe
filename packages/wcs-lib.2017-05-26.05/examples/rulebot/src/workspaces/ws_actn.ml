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

let intents = [
  intent "mk_define"
    ~examples: [ "create definition";
                 "create new";
                 "define";
                 "definition";
                 "new definition";
                 "new variable";
                 "variable"; ]
    ();
  intent "mk_emit"
    ~examples: [ "emit";
                 "emitting";
                 "emitting event";
                 "output event";
                 "trigger";
                 "trigger new event"; ]
    ();
  intent "mk_print"
    ~examples: [ "output string";
                 "print";
                 "print action";
                 "printing";
                 "stdout";
                 "string" ]
    ();
  intent "mk_set"
    ~examples: [ "initialize field";
                 "set";
                 "set field";
                 "set field of variable";
                 "set variable field"; ]
    ();
  Ws_common.intent_help;
]

let ws_actn bmd =
  let entities =
    Ws_common.entity_yes ::
    Ws_common.entity_no ::
    (Bmd_to_wcs_entities.entities_of_bmd bmd) in


  let ws_nodes = Ws_common.make_top_dialog begin fun (module SD) -> let open SD in
                   let print_action =
                     let instr = A_print (mk_expr_undefined ()) in
                     dialog_node "Print Action"
                       ~description: "Specify a when clause"
                       ~conditions: "#mk_print && intents[0].confidence > 0.9"
                       ~text: "Ok, I'm adding a new print action."
                       ~context: (Ctx.set_actn_desc
                                    (Ctx.set_skip_user_input `Null true)
                                    "actn_desc" instr)
                       ()
                   in add_node print_action ;

                   let emit_action =
                     let instr = A_emit (mk_expr_undefined ()) in
                     dialog_node "Emit Action"
                       ~conditions: "#mk_emit && intents[0].confidence > 0.9"
                       ~text: "Ok, I'm adding a new emit action."
                       ~context: (Ctx.set_actn_desc
                                    (Ctx.set_skip_user_input `Null true)
                                    "actn_desc" instr)
                       ()
                   in add_node emit_action ;

                   let define_action_filled =
                     let instr = A_define ("$has_string", mk_expr_undefined ()) in
                     dialog_node "Define Action Filled"
                       ~conditions: "#mk_define && intents[0].confidence > 0.9  && $has_string"
                       ~text: "Ok, I'm adding a new definition for variable '$has_string'."
                       ~context: (Ctx.set_actn_desc
                                    (Ctx.set_skip_user_input `Null false)
                                    "actn_desc" instr)
                       ()
                   in add_node define_action_filled ;

                   let define_action =
                     dialog_node "Define Action"
                       ~text: "Ok, let's add a define action, what is the name of the variable to be defined?"
                       ~conditions: "#mk_define && intents[0].confidence > 0.9"
                       ()
                   in add_node define_action ;

                   let variable_name =
                     let instr = A_define ("<? input.text ?>", mk_expr_undefined ()) in
                     dialog_node "Variable Name"
                       ~parent: define_action
                       ~text: "Ok, I'm adding a new definition for variable '<? input.text ?>'."
                       ~context: (Ctx.set_actn_desc
                                    (Ctx.set_skip_user_input `Null false)
                                    "actn_desc" instr)
                       ()
                   in add_node variable_name ;
                   let set_action =
                     dialog_node "Set Action"
                       ~conditions: "#mk_set && intents[0].confidence > 0.9"
                       ~text: "Ok, let's add a set action, what is the name of the corresponding variable?"
                       ()
                   in
                   let set_action_cont =
                     dialog_node "Set Action cont"
                       ~parent: set_action
                       ~text: "..and then what field of variable '<? input.text ?>' should be set?"
                       ~context: (`Assoc [ "variable", `String "<? input.text ?>" ])
                       ()
                   in
                   let field_name =
                     let instr = A_set ("@field", "$variable", mk_expr_undefined ()) in
                     dialog_node "Field Name"
                       ~parent: set_action_cont
                       ~text: "Ok, I'm adding a new set action for the field @field of variable '$variable'."
                       ~conditions: "entities['field']"
                       ~context: (Ctx.set_actn_desc
                                    (Ctx.set_skip_user_input `Null false)
                                    "actn_desc" instr)
                       ()
                   in
                   let set_action_filled =
                     dialog_node "Set Action Filled"
                       ~conditions: "#mk_set && intents[0].confidence > 0.9 && $has_string"
                       ~text: "Ok, let's add a set action for '$has_string'. Which field of '$has_string' should be set?"
                       ~context: (`Assoc [ "variable", `String "$has_string" ])
                       ~next_step: (field_name, Goto_user_input)
                       ()
                   in add_node set_action_filled; add_node set_action ; add_node set_action_cont ; add_node field_name ;

                   add_usage "Set field" "which field" set_action_cont (Ws_common.bmd_fields bmd);

                   let prompt =
                     dialog_node "Prompt"
                       ~conditions: "conversation_start"
                       ~text: "What kind of action would you like to add?"
                       ()
                   in add_node prompt ;

                   let help =
                     dialog_node "Help"
                       ~conditions: "#mk_help && intents[0].confidence > 0.9"
                       ~text: "The available actions are print, emit, define or set."
                       ~next_step: (prompt, Goto_body)
                       ()
                   in add_node help ;

                   let anything_else =
                     dialog_node "Anything else"
                       ~text: "I don't understand."
                       ~conditions: "anything_else"
                       ~next_step: (help, Goto_body)
                       ()
                   in add_node anything_else
                 end in

  workspace "rulebot-actn"
    ~description: "Dialog to build the action part of a rule"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: ws_nodes
    ()
