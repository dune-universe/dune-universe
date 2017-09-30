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

(** {6. The [list] command} *)

let list_page_limit = ref None
let set_list_page_limit n =
  list_page_limit := Some n

let list_include_count = ref None
let set_list_include_count b =
  list_include_count := Some b

let list_sort = ref None
let set_list_sort s =
  list_sort :=
    Some (Wcs_j.sort_workspace_criteria_of_string
            (Yojson.Basic.to_string (`String s)))

let list_cursor = ref None
let set_list_cursor s =
  list_cursor := Some s

let list_short = ref false
let set_list_short () =
  list_short := true

let list_speclist =
  [ "-page_limit", Arg.Int set_list_page_limit,
    "n The number of records to return in each page of results.";
    "-include_count", Arg.Bool set_list_include_count,
    "b Whether to include information about the number of records returned.";
    "-sort", Arg.String set_list_sort,
    "attr The attribute by which returned results will be sorted. To reverse the sort order, prefix the value with a minus sign (-). Supported values are name, modified, and workspace_id.";
    "-cursor", Arg.String set_list_cursor,
    "token A token identifying the last value from the previous page of results.";
    "-short", Arg.Unit set_list_short,
    " Display ony workspace ids and names (set by default by the ls command).";
  ]

let list_anon_args s =
  Log.warning "Wcs_cli" ("ignored argument: " ^ s)


let list_usage =
  "Usage:\n"^
  "  "^cmd_name^" (list | ls) [options]"^"\n"^
  "Options:"

let list wcs_cred =
  let req =
    Wcs_builder.list_workspaces_request
      ?page_limit:!list_page_limit
      ?include_count:!list_include_count
      ?sort:!list_sort
      ?cursor:!list_cursor
      ()
  in
  let rsp = Wcs.list_workspaces wcs_cred req in
  begin match !list_short with
  | false ->
      Format.printf "%s@." (Wcs_json.pretty_list_workspaces_response rsp)
  | true ->
      List.iter
        (fun ws ->
           let name =
             begin match ws.ws_rsp_name with
             | Some n -> n
             | None -> ""
             end
           in
           Format.printf "%s %s@."
             ws.ws_rsp_workspace_id name)
        rsp.list_ws_rsp_workspaces
  end

(** {6. The [create] command} *)

let create_ws_fnames = ref []

let create_speclist = []

let create_anon_args s =
  create_ws_fnames := !create_ws_fnames @ [ s ]

let create_usage =
  "Usage:\n"^
  "  "^cmd_name^" create [options] [workspace.json ...]\n"^
  "Options:"

let create wcs_cred =
  List.iter
    (fun fname ->
       let ws =
         Json.read_json_file Wcs_j.read_workspace fname
       in
       let rsp = Wcs.create_workspace wcs_cred ws in
       let name =
         begin match rsp.crea_rsp_name with
         | Some name -> name
         | None -> "?"
         end
       in
       let ws_id =
         begin match rsp.crea_rsp_workspace_id with
         | Some id -> id
         | None -> "?"
         end
       in
       Format.printf "Workspace %s: %s@." name ws_id)
    !create_ws_fnames


(** {6. The [delete] command} *)

let delete_ws_ids = ref []

let delete_speclist =
  [ ]

let delete_anon_args s =
  delete_ws_ids := !delete_ws_ids @ [ s ]

let delete_usage =
  "Usage:\n"^
  "  "^cmd_name^" delete [options] [workspace_id ...]\n"^
  "Options:"

let delete wcs_cred =
  List.iter
    (fun id ->
       Wcs.delete_workspace wcs_cred id;
       Format.printf "Workspace %s deleted@." id)
    !delete_ws_ids


(** {6. The [get] command} *)

let get_export = ref None
let set_get_export () =
  get_export := Some true

let get_ws_ids = ref []

let get_speclist =
  [ "-export", Arg.Unit set_get_export,
    " To include all element content in the returned data.";]

let get_anon_args s =
  get_ws_ids := !get_ws_ids @ [ s ]

let get_usage =
  "Usage:\n"^
  "  "^cmd_name^" get [options] [workspace_id ...]\n"^
  "Options:"

let get wcs_cred =
  let workspaces =
    List.fold_left
      (fun acc id ->
         let req =
           Wcs_builder.get_workspace_request ?export:!get_export id
         in
         let ws = Wcs.get_workspace wcs_cred req in
         (Wcs_json.json_of_workspace ws) :: acc)
      [] !get_ws_ids
  in
  begin match workspaces with
  | [ ws ] ->
      Format.printf "%s@."
        (Yojson.Basic.pretty_to_string ws)
  | workspaces ->
      Format.printf "%s@."
        (Yojson.Basic.pretty_to_string (`List (List.rev workspaces)))
  end


(** {6. The [update] command} *)

let update_ws_fname = ref None

let update_ws_id = ref None

let update_speclist =
  [ ]

let update_anon_args =
  let cpt = ref 0 in
  begin fun s ->
    incr cpt;
    begin match !cpt with
    | 1 -> update_ws_fname := Some s
    | 2 -> update_ws_id := Some s
    | _ -> Log.warning "Wcs_cli" ("ignored argument: " ^ s)
    end
  end

let update_usage =
  "Usage:\n"^
  "  "^cmd_name^" update [options] workspace.json workspace_id\n"^
  "Options:"

let update wcs_cred =
  begin match !update_ws_id, !update_ws_fname with
  | Some id, Some fname ->
      let ws =
        Json.read_json_file Wcs_j.read_workspace fname
      in
      Wcs.update_workspace wcs_cred id ws
  | _ ->
      let usage =
        Format.sprintf "%s update: workspace file and workspace id required"
          cmd_name
      in
      Log.error "Wcs_cli" (Some ())
        (Arg.usage_string update_speclist usage)
  end


(** {6. The [logs] command} *)

let logs_ws_ids = ref []

let logs_filter = ref None
let set_logs_filter b =
  logs_filter := Some b

let logs_sort = ref None
let set_logs_sort s =
  logs_sort :=
    Some (Wcs_j.sort_logs_criteria_of_string
            (Yojson.Basic.to_string (`String s)))

let logs_page_limit = ref None
let set_logs_page_limit n =
  logs_page_limit := Some n

let logs_cursor = ref None
let set_logs_cursor s =
  logs_cursor := Some s

let logs_speclist =
  [ "-filter", Arg.String set_logs_filter,
    "s A cacheable parameter that limits the results to those matching the specified filter.";
    "-page_limit", Arg.Int set_logs_page_limit,
    "n The number of records to return in each page of results.";
    "-sort", Arg.String set_logs_sort,
    "attr The attribute by which returned results will be sorted. To reverse the sort order, prefix the value with a minus sign (-). The only supported value is request_timestamp.";
    "-cursor", Arg.String set_logs_cursor,
    "token A token identifying the last value from the previous page of results.";
  ]

let logs_anon_args s =
  logs_ws_ids := !logs_ws_ids @ [s]

let logs_usage =
  "Usage:\n"^
  "  "^cmd_name^" logs [options] [workspace_id ...]"^"\n"^
  "Options:"

let logs wcs_cred =
  List.iter
    (fun id ->
       let req =
         Wcs_builder.logs_request
           ?filter:!logs_filter
           ?sort:!logs_sort
           ?page_limit:!logs_page_limit
           ?cursor:!logs_cursor
           ()
       in
       let rsp = Wcs.logs wcs_cred id req in
       Format.printf "%s@." (Wcs_json.pretty_logs_response rsp))
    !logs_ws_ids


(** {6. The [try] command} *)

let try_context = ref `Null
let set_try_context fname =
  begin try
    let ctx =
      Yojson.Basic.from_file ~fname:fname fname
    in
    try_context := ctx
  with
  | exn ->
      Log.error "Wcs_cli" (Some ())
        ("Unable to read the context file "^fname^": "^
         Printexc.to_string exn)
  end
let try_text = ref ""
let set_try_text txt =
  try_text := txt

let try_node = ref None
let set_try_node node_id =
  try_node := Some node_id

let try_ws_id = ref None

let try_speclist =
  [ "-context", Arg.String set_try_context,
    "ctx.json The initial context.";
    "-text", Arg.String set_try_text,
    "txt The initial user input.";
    "-node", Arg.String set_try_node,
    "node_id The node where to start the conversation.";
  ]

let try_anon_args =
  let first = ref true in
  begin fun s ->
    if !first then begin
      try_ws_id := Some s;
      first := false
    end else begin
      Log.warning "Wcs_cli" ("ignored argument: " ^ s)
    end
  end

let try_usage =
  "Usage:\n"^
  "  "^cmd_name^" try [options] workspace_id\n"^
  "Options:"

let try_ wcs_cred =
  let ws_main_id =
    begin match !try_ws_id with
    | Some id -> id
    | None ->
        let usage =
          Format.sprintf "%s try: workspace id required"
            cmd_name
        in
        Log.error "Wcs_cli" None
          (Arg.usage_string update_speclist usage)
    end
  in
  ignore (Wcs_bot.exec wcs_cred ws_main_id !try_context !try_text)


(** Select command *)

let wcs_credential : Wcs_t.credential option ref = ref None
let set_wcs_credential f =
  let cred = Json.read_json_file Wcs_j.read_credential f in
  wcs_credential := Some cred

let command = ref Cmd_nothing

let unset_error_recovery () =
  Log.error_recovery := false

let set_debug () =
  Log.debug_message := true

let print_version () =
  Format.printf "Watson Conversation Service API %s@." Wcs.version

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
        list_usage
  | "create" ->
      command := Cmd_create;
      Arg.parse_argv Sys.argv
        (Arg.align (create_speclist @ speclist))
        create_anon_args
        create_usage
  | "delete" | "rm" ->
      command := Cmd_delete;
      Arg.parse_argv Sys.argv
        (Arg.align (delete_speclist @ speclist))
        delete_anon_args
        delete_usage
  | "get" ->
      command := Cmd_get;
      Arg.parse_argv Sys.argv
        (Arg.align (get_speclist @ speclist))
        get_anon_args
        get_usage
  | "update" ->
      command := Cmd_update;
      Arg.parse_argv Sys.argv
        (Arg.align (update_speclist @ speclist))
        update_anon_args
        update_usage
  | "logs" ->
      command := Cmd_logs;
      Arg.parse_argv Sys.argv
        (Arg.align (logs_speclist @ speclist))
        logs_anon_args
        logs_usage
  | "try" ->
      command := Cmd_try;
      Arg.parse_argv Sys.argv
        (Arg.align (try_speclist @ speclist))
        try_anon_args
        try_usage
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
  "  list|ls    List the workspaces associated with a Conversation service instance.\n"^
  "  create     Create workspaces on the Conversation service instance.\n"^
  "  delete|rm  Delete workspaces from the Conversation service instance.\n"^
  "  get        Get information about workspaces, optionally including all workspace contents.\n"^
  "  update     Update an existing workspace with new or modified data.\n"^
  "  logs       List the events from the log of a workspace.\n"^
  "  try        Generic bot running in the terminal.\n"^
  "\n"^
  "Options:"


let main () =
  let () =
    begin try
      let cred_file = Sys.getenv "WCS_CRED" in
      set_wcs_credential cred_file
    with
    | Not_found -> ()
    | exn ->
        Log.error "Wcs_cli" (Some ()) ("WCS_CRED: "^(Printexc.to_string exn))
    end
  in
  Arg.parse speclist anon_args usage;
  let wcs_cred =
    begin match !wcs_credential with
    | Some ws_cred -> ws_cred
    | _ ->
        Arg.usage speclist ("Option -wcs-cred or WCS_CRED environment variable is required\n"^usage);
        exit 0
    end
  in
  begin match !command with
  | Cmd_nothing -> ()
  | Cmd_list -> list wcs_cred
  | Cmd_create -> create wcs_cred
  | Cmd_delete -> delete wcs_cred
  | Cmd_get -> get wcs_cred
  | Cmd_update -> update wcs_cred
  | Cmd_logs -> logs wcs_cred
  | Cmd_try -> try_ wcs_cred
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
