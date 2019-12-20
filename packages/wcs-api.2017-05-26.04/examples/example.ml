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
open Wcs_api
open Wcs_t

let intent_help =
  Wcs.intent "help"
    ~description:"The user needs help knowing what to do"
    ~examples:[ "help";
                "I am confused";
                "What can I do";
                "What are my choices";
                "options";
                "alternatives";
                "choices";
                "usage";
                "guide";
                "directions"; ]
    ()

let is_number =
  Wcs.dialog_node "Is number"
    ~conditions_spel: (Spel.entity Wcs.sys_number ())
    ~text: "You have selected the number @sys-number."
    ~context: (Context.skip_user_input true)
    ()

let win =
  Wcs.dialog_node "Win"
    ~parent: is_number
    ~conditions: "@sys-number == 42"
    ~text: "You win!"
    ~context: (Context.return (Json.bool true))
    ()

let lost =
  Wcs.dialog_node "Lost"
    ~previous_sibling: win
    ~conditions: "anything_else"
    ~text: "Sorry, do you want to try again?"
    ()


let mk_prompt prompt_txt help_txt =
  let prompt =
    Wcs.dialog_node "Prompt"
      ~conditions: "conversation_start"
      ~text: prompt_txt
      ()
  in
  let help =
    Wcs.dialog_node "Help"
      ~conditions: "#help"
      ~text: help_txt
      ~next_step: (prompt, Goto_body)
      ()
  in
  let default =
    Wcs.dialog_node "Default"
      ~text: "I don't understand what you say."
      ~next_step: (help, Goto_body)
      ()
  in
  [ prompt; help; default; ]

let example1 =
  Wcs.workspace "Example 1"
    ~intents: [ intent_help ]
    ~entities: [ Wcs.sys_number ]
    ~dialog_nodes: (is_number
                    :: win
                    :: lost
                    :: (mk_prompt
                          "Enter a number."
                          "It is a game where you have to guest a number."))
    ()



let main () =
  let wcs_cred_file = ref None in
  let ws_id = ref None in
  let print = ref false in
  let deploy = ref false in
  let exec = ref false in
  let speclist =
    Arg.align
      [ "-cred", Arg.String (fun s -> wcs_cred_file := Some s),
        "cred.json The file containing the Watson Conversation Service credentials.";
        "-id", Arg.String (fun id -> ws_id := Some id),
        "id The workspace id used to update in conjunction with -deploy.";
        "-print", Arg.Set print,
        " Print the workspace on stdout.";
        "-deploy", Arg.Set deploy,
        " Create or update the workspace on Watson Conversation Service.";
        "-exec", Arg.Set exec,
        " Execute the chatbot.";
        "-debug", Arg.Set Log.debug_message,
        " Print debug messages.";
      ]
  in
  let usage =
    "Usage: "^Sys.argv.(0)^" [options]"
  in
  Arg.parse speclist (fun _ -> ()) usage;
  let wcs_cred = Wcs_bot.get_credential !wcs_cred_file in
  begin match !print with
  | true ->
      print_endline (Wcs_pretty.workspace example1)
  | false ->
      ()
  end;
  begin match !deploy, !ws_id with
  | true, Some ws_id ->
      let () = Wcs_call.update_workspace wcs_cred ws_id example1 in
      Format.printf "%s: updated@." ws_id
  | true, None ->
      begin match Wcs_call.create_workspace wcs_cred example1 with
      | { crea_rsp_workspace_id = Some id } ->
          Format.printf "%s: created@." id;
          ws_id := Some id;
      | _ -> assert false
      end
  | false, _ -> ()
  end;
  begin match !exec, !ws_id with
  | true, Some id ->
      let _ = Wcs_bot.exec wcs_cred id (`Assoc [ "fact", `String id]) "" in
      ()
  | false, _ ->
      ()
  | true, None ->
      Arg.usage speclist "no worksapce to execute";
      exit 1
  end

let _ =
  begin try
    main ()
  with
  | Log.Error (module_name, msg) when not !Log.debug_message ->
      Format.eprintf "%s@." msg;
      exit 1
  end
