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

let ws_fact =
  let fact = Spel.variable "fact" in
  let n = Spel.variable "n" in
  let return = Spel.variable "return" in
  let res = Spel.variable "res" in
  let base =
    Wcs.dialog_node "Base"
      ~conditions_spel:
        (Spel.or_
           (Spel.eq n (Spel.int 0))
           (Spel.eq n (Spel.int 1)))
      ~text_spel:
        (Spel.concat [Spel.string "fact "; n; Spel.string " = "; n])
      ~context_spel:
        (Context_spel.return (`Expr n))
      ()
  in
  let inductif =
    Wcs.dialog_node "Inductif"
      ~conditions_spel: (Spel.gt n (Spel.int 1))
      ~text_spel:
        (Spel.concat [Spel.string "Let's compute fact "; n])
      ~context_spel:
        (Json_spel.assign
           [ Context_spel.skip_user_input true;
             Context_spel.actions_def
               [ Wcs.action_def (Spel_print.to_string fact)
                   ~parameters:
                     (`Assoc [
                        "context", (`Assoc [
                          ("n", `Expr (Spel.minus n (Spel.int 1)));
                          ("fact", `Expr fact)
                        ])
                      ])
                   ~result_variable: "context.res"
                   ()
               ]])
      ()
  in
  let compute =
    Wcs.dialog_node "Compute"
      ~parent: inductif
      ~conditions_spel: (Spel.bool true)
      ~context_spel: (Context_spel.return (`Expr (Spel.mult res n)))
      ~text_spel:
        (Spel.concat [Spel.string "fact "; n; Spel.string " = "; return])
      ()
  in
  let start =
    Wcs.dialog_node "Start"
      ~conditions_spel: (Spel.entity Wcs.sys_number ())
      ~text_spel:
        (Spel.concat [Spel.string "Start computation of fact ";
                      Spel.entity Wcs.sys_number ()])
      ~context_spel:
        (Json_spel.assign
           [ `Assoc [ "n", `Expr (Spel.entity Wcs.sys_number ()) ];
             Context_spel.skip_user_input true;
             Context_spel.actions_def
               [ Wcs.action_def (Spel_print.to_text fact)
                   ~parameters:
                     (`Assoc [
                        "context", (`Assoc [
                          ("n", `Expr (Spel.entity Wcs.sys_number ()));
                          ("fact", `Expr fact)
                        ])
                      ])
                   ~result_variable: "context.res"
                   ()
               ];
           ])
      ()
  in
  let finish =
    Wcs.dialog_node "Finish"
      ~parent: start
      ~text_spel:
        (Spel.concat [Spel.string "Final result: fact "; n;
                      Spel.string " = "; Spel.variable "res";])
      ~context_spel:
        (Context_spel.return (`Expr (Spel.variable "res")))
      ()
  in
  let help =
    Wcs.dialog_node "Help"
      ~conditions_spel: Spel.anything_else
      ~text: "Enter a positive number"
      ()
  in
  Wcs.workspace "fact"
    ~entities: [Wcs.sys_number]
    ~dialog_nodes: [
      base;
      inductif;
      compute;
      start;
      finish;
      help;
    ]
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
      print_endline (Wcs_pretty.workspace ws_fact)
  | false ->
      ()
  end;
  begin match !deploy, !ws_id with
  | true, Some ws_id ->
      let () = Wcs_call.update_workspace wcs_cred ws_id ws_fact in
      Format.printf "%s: updated@." ws_id
  | true, None ->
      begin match Wcs_call.create_workspace wcs_cred ws_fact with
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
