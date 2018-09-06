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
open Wcs_cmd

type command =
  | Cmd_nothing
  | Cmd_list
  | Cmd_create
  | Cmd_delete
  | Cmd_get
  | Cmd_update
  | Cmd_logs
  | Cmd_try

let cmd_name = Sys.argv.(0)

(** Select command *)

let wcs_cred : string option ref = ref None
let set_wcs_credential f =
  wcs_cred := Some f

let command = ref Cmd_nothing

let unset_error_recovery () =
  Log.error_recovery := false

let set_debug () =
  Log.debug_message := true

let print_version () =
  Format.printf "Watson Conversation Service API %s@." Wcs_call.version

let speclist =
  [ "-wcs-cred", Arg.String set_wcs_credential,
    "cred.json The file containing the Watson Conversation Service credentials.";
    "-version", Arg.Unit print_version,
    " Print the Watson Conversation API version number used.";
    "-no-error-recovery", Arg.Unit unset_error_recovery,
    " Do not try to recover in case of error.";
    "-debug", Arg.Unit set_debug,
    " Print debug messages.";
  ]

let set_command cmd =
  begin match cmd with
  | "list" | "ls" ->
      if cmd = "ls" then set_list_short ();
      command := Cmd_list;
      Arg.parse_argv Sys.argv
        (Arg.align (list_speclist @ speclist))
        list_anon_args
        (list_usage cmd_name)
  | "create" ->
      command := Cmd_create;
      Arg.parse_argv Sys.argv
        (Arg.align (create_speclist @ speclist))
        create_anon_args
        (create_usage cmd_name)
  | "delete" | "rm" ->
      command := Cmd_delete;
      Arg.parse_argv Sys.argv
        (Arg.align (delete_speclist @ speclist))
        delete_anon_args
        (delete_usage cmd_name)
  | "get" ->
      command := Cmd_get;
      Arg.parse_argv Sys.argv
        (Arg.align (get_speclist @ speclist))
        get_anon_args
        (get_usage cmd_name)
  | "update" ->
      command := Cmd_update;
      Arg.parse_argv Sys.argv
        (Arg.align (update_speclist @ speclist))
        update_anon_args
        (update_usage cmd_name)
  | "logs" ->
      command := Cmd_logs;
      Arg.parse_argv Sys.argv
        (Arg.align (logs_speclist @ speclist))
        logs_anon_args
        (logs_usage cmd_name)
  | "try" ->
      command := Cmd_try;
      Arg.parse_argv Sys.argv
        (Arg.align (try_speclist @ speclist))
        try_anon_args
        (try_usage cmd_name)
  | _ ->
      let msg =
        Format.sprintf "'%s' is not a %s command" cmd cmd_name
      in
      raise (Arg.Bad msg)
  end

let anon_args s =
  begin try
    set_command s
  with
  | Arg.Bad msg
  | Arg.Help msg ->
      Format.eprintf "%s@." msg;
      exit 0
  end

let usage =
  "Usage:\n"^
  "  "^cmd_name^" command [options]\n"^
  "\n"^
  "Available Commands:\n"^
  "  list       Return the workspaces associated with a Conversation service instance in JSON format.\n"^
  "  ls         List workspaces IDs and names for a Conversation service instance.\n"^
  "  create     Create workspaces on the Conversation service instance.\n"^
  "  delete|rm  Delete workspaces from the Conversation service instance.\n"^
  "  get        Get information about workspaces, optionally including all workspace contents.\n"^
  "  update     Update an existing workspace with new or modified data.\n"^
  "  logs       List the events from the log of a workspace.\n"^
  "  try        Generic bot running in the terminal.\n"^
  "\n"^
  "Options:"


let main () =
  Arg.parse (Arg.align speclist) anon_args usage;
  let wcs_credential =
    begin try
      Wcs_bot.get_credential !wcs_cred
    with Log.Error (_, msg) ->
      Arg.usage speclist
        (msg ^
         "\nOption -wcs-cred or WCS_CRED environment variable is required\n" ^
         usage);
      exit 0
    end
  in
  begin match !command with
  | Cmd_nothing -> ()
  | Cmd_list -> list wcs_credential
  | Cmd_create -> create wcs_credential
  | Cmd_delete -> delete wcs_credential
  | Cmd_get -> get wcs_credential
  | Cmd_update -> update wcs_credential cmd_name
  | Cmd_logs -> logs wcs_credential
  | Cmd_try -> try_ wcs_credential cmd_name
  end;
  ()

let _ =
  begin try
    main ()
  with
  | Log.Error (module_name, msg) when not !Log.debug_message ->
      Format.eprintf "%s@." msg;
      exit 1
  end
