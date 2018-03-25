
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

open Result
open Lwt.Infix
open Protocol_j

module M = Message

exception Restart

(** {2 Prelude} *)

type 'a or_error = ('a, string) result

type json = Yojson.Safe.json

type mime_data = {
  mime_type: string;
  mime_content: string; (* raw content *)
  mime_b64: bool; (* if true, content will be encoded with base64 *)
}

(* list of mime objects, all of distinct types *)
type mime_data_bundle = mime_data list

module Kernel = struct
  type exec_action =
    | Mime of mime_data_bundle

  type exec_status_ok = {
    msg: string option;
    (* main message *)
    actions: exec_action list;
    (* other actions *)
  }

  type completion_status = {
    completion_matches: string list;
    completion_start: int;
    completion_end: int;
  }

  type is_complete_reply =
    | Is_complete
    | Is_not_complete of string (* indent *)

  type history_request = Protocol_j.history_request

  type inspect_request = Protocol_j.inspect_request = {
    ir_code: string;
    ir_cursor_pos: int; (* cursor pos *)
    ir_detail_level: int; (* 0 or 1 *)
  }

  type inspect_reply_ok = {
    iro_status: string; (* "ok" or "error" *)
    iro_found: bool;
    iro_data: mime_data_bundle;
  }

  let mime ?(base64=false) ~ty x =
    Mime [{mime_type=ty; mime_content=x; mime_b64=base64}]

  let ok ?(actions=[]) msg = {msg; actions}

  type t = {
    init: unit -> unit Lwt.t;
    exec: count:int -> string -> exec_status_ok or_error Lwt.t; (* TODO: user expressions *)
    is_complete: string -> is_complete_reply Lwt.t;
    language: string;
    language_version: int list;
    banner: string option; (* displayed at startup *)
    file_extension: string;
    mime_type: string option; (* default: text/plain *)
    codemirror_mode: string option;
    complete: pos:int -> string -> completion_status Lwt.t;
    inspect: inspect_request -> inspect_reply_ok or_error Lwt.t;
    history: history_request -> string list Lwt.t;
  }

  let make
      ?banner
      ?(file_extension=".txt")
      ?mime_type
      ?codemirror_mode
      ?(init=fun () -> Lwt.return_unit)
      ?(is_complete=fun _ -> Lwt.return Is_complete)
      ?(complete=fun ~pos i->
        Lwt.return {completion_matches=[]; completion_start=pos;completion_end=pos})
      ?(inspect=fun _ -> Lwt.return (Error "no inspection implemented"))
      ?(history=fun _ -> Lwt.return [])
      ~language_version
      ~language
      ~exec
      () : t =
    { banner; file_extension; mime_type; language; language_version;
      is_complete; history; exec; complete; inspect; init; codemirror_mode
    }
end

type t = {
  sockets: Sockets.t;
  key: string option;
  kernel: Kernel.t;
  mutable e_count: int;
}

let make ?key sockets kernel : t =
  { key; sockets; kernel; e_count=0; }

type iopub_message =
  | Iopub_send_message of Message.content
  | Iopub_send_mime of mime_data_bundle

let string_of_message content =
  let msg_type = M.msg_type_of_content content in
  Printf.sprintf "{`%s` content `%s`}"
    msg_type (M.json_of_content content)

let string_of_mime_data (m:mime_data): string =
  Printf.sprintf "mime{ty=%s; content=%S; base64=%B}"
    m.mime_type m.mime_content m.mime_b64

let string_of_mime_data_bundle l : string =
  "[" ^ String.concat "\n" (List.map string_of_mime_data l) ^ "]"

let string_of_iopub_message = function
  | Iopub_send_message content ->
    Printf.sprintf "send_message %s" (string_of_message content)
  | Iopub_send_mime l ->
    Printf.sprintf "send_mime %s" (string_of_mime_data_bundle l)

let dict_of_mime_bundle (l:mime_data_bundle): json =
  let l =
    l
    |> List.map (fun m ->
      let data =
        if not m.mime_b64 then m.mime_content
        else Base64.encode m.mime_content
      in
      m.mime_type, `String data)
  in
  `Assoc l

(* encode mime data, wrap it into a message *)
let mime_message_content (m:mime_data_bundle) : M.content =
  Message.Display_data (Protocol_j.({
       dd_data = dict_of_mime_bundle m;
       dd_metadata = `Assoc [];
       dd_transient=None; (* TODO *)
     }))

let send_shell (t:t) ~parent (content:M.content): unit Lwt.t =
  Log.logf "send_shell `%s`\n" (string_of_message content);
  let socket = t.sockets.Sockets.shell in
  let msg_type = M.msg_type_of_content content in
  let msg' = M.make ~parent ~msg_type content in
  M.send ?key:t.key socket msg'

(* send a message on the Iopub socket *)
let send_iopub (t:t) ?parent (m:iopub_message): unit Lwt.t =
  Log.logf "send_iopub `%s`\n" (string_of_iopub_message m);
  let socket = t.sockets.Sockets.iopub in
  let send_message msg_type content =
    let msg' = match parent with
      | None -> M.make_first ~msg_type content
      | Some parent -> M.make ~parent ~msg_type content
    in
    M.send ?key:t.key socket msg'
  in
  let send_mime (l:mime_data_bundle) =
    (* send mime message *)
    let content = mime_message_content l in
    let msg_type = M.msg_type_of_content content in
    send_message msg_type content
  in
  begin match m with
    | Iopub_send_message content ->
      let msg_type = M.msg_type_of_content content in
      send_message msg_type content
    | Iopub_send_mime l ->
      send_mime l
  end

(* run [f ()] in a "status:busy" context *)
let within_status (t:t) ~f =
  (* set state to busy *)
  let%lwt _ =
    send_iopub t
      (Iopub_send_message (M.Status { execution_state = "busy" }))
  in
  Lwt.finalize
    f
    (fun () ->
       send_iopub t
         (Iopub_send_message
          (M.Status { execution_state = "idle" })))

(* execute code *)
let execute_request (t:t) ~parent e : unit Lwt.t =
  (* if we are not silent increment execution count *)
  if not e.silent then (
    t.e_count <- t.e_count + 1;
  );

  let execution_count = t.e_count in

  let%lwt _ = send_iopub t ~parent
      (Iopub_send_message
        (M.Execute_input {
            pi_code = e.code;
            pi_execution_count = execution_count;
          }))
  in

  (* eval code *)
  let%lwt status = t.kernel.Kernel.exec ~count:execution_count e.code in

  (* in case of success, how to print *)
  let reply_status_ok (s:string option) = match s with
    | None -> Lwt.return_unit
    | Some msg ->
      send_iopub t ~parent (Iopub_send_message
          (M.Execute_result {
              po_execution_count = execution_count;
              po_data = `Assoc ["text/plain", `String msg];
              po_metadata = `Assoc []; }))
      >|= fun _ -> ()
  and side_action (s:Kernel.exec_action) : unit Lwt.t = match s with
    | Kernel.Mime l -> send_iopub t ~parent (Iopub_send_mime l)
  in
  let%lwt () = match status with
    | Ok ok ->
      let%lwt () =
        send_shell t ~parent
          (M.Execute_reply {
              status = "ok";
              execution_count;
              ename = None; evalue = None; traceback = None; payload = None;
              er_user_expressions = None;
            })
      in
      let%lwt _ = reply_status_ok ok.Kernel.msg in
      (* send mime type in the background *)
      Lwt_list.iter_p side_action ok.Kernel.actions
    | Error err_msg ->
      let content =
        M.Execute_reply {
            status = "error";
            execution_count;
            ename = Some "error"; evalue = Some err_msg;
            traceback = Some ["<eval>"]; payload = None;
            er_user_expressions = None;
        }
      in
      Log.logf "send ERROR `%s`\n" (M.json_of_content content);
      let%lwt () = send_shell t ~parent content in
      send_iopub t ~parent
        (Iopub_send_message (M.Execute_error {
             err_ename="error";
             err_evalue=err_msg;
             err_traceback=[
               "ERROR " ^ err_msg;
               "evaluating " ^ e.code;
             ];
           }))
  in
  Lwt.return_unit

let kernel_info_request (t:t) ~parent =
  let str_of_version l = String.concat "." (List.map string_of_int l) in
  let%lwt _ =
    send_shell t ~parent (M.Kernel_info_reply {
        implementation = t.kernel.Kernel.language;
        implementation_version =
          str_of_version t.kernel.Kernel.language_version;
        protocol_version = "5.0";
        language_info = {
          li_name = t.kernel.Kernel.language;
          li_version = str_of_version t.kernel.Kernel.language_version;
          li_mimetype=(match t.kernel.Kernel.mime_type with
            | Some m -> m
            | None -> "text"
          );
          li_file_extension=t.kernel.Kernel.file_extension;
          li_codemirror_mode=t.kernel.Kernel.codemirror_mode;
        };
        banner= (match t.kernel.Kernel.banner with
          | None -> ""
          | Some b -> b);
        help_links=[];
      })
  in
  Lwt.return_unit

let comm_info_request (t:t) ~parent =
  let%lwt _ =
    send_shell t ~parent M.Comm_info_reply
  in
  Lwt.return_unit

let shutdown_request (t:t) ~parent (r:shutdown) : 'a Lwt.t =
  Log.log "received shutdown request...\n";
  let%lwt () =
    Lwt.catch
      (fun () -> send_shell t ~parent (M.Shutdown_reply r))
      (fun e ->
         Log.logf "exn %s when replying to shutdown request" (Printexc.to_string e);
         Lwt.return_unit)
  in
  Lwt.fail (if r.restart then Restart else Exit)

let handle_invalid_message () =
  Lwt.fail (Failure "Invalid message on shell socket")

(* translate positions in codepoints, to positions in bytes offset *)
let byte_pos_of_utf_pos s ~cursor_pos : int =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec iter n =
    if n=0 then Uutf.decoder_byte_count dec (* done *)
    else match Uutf.decode dec with
      | `Await -> assert false
      | `End -> String.length s + 1 (* out of range *)
      | `Malformed _ ->
        (* skip malformed substring *)
        iter (n-1)
      | `Uchar _ ->
        iter (n-1)
  in
  assert (cursor_pos >= 0);
  iter cursor_pos

(* translate positions in byte offsets, to positions in codepoints *)
let utf_pos_of_byte_pos s ~pos : int =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec iter n =
    if Uutf.decoder_byte_count dec >= pos
    then Uutf.decoder_count dec (* done *)
    else match Uutf.decode dec with
      | `Await -> assert false
      | `End -> Uutf.decoder_count dec + 1 (* out of range *)
      | `Malformed _ ->
        (* skip malformed substring *)
        iter (n-1)
      | `Uchar _ ->
        iter (n-1)
  in
  assert (pos >= 0);
  iter pos

let complete_request t ~parent (r:complete_request): unit Lwt.t =
  let%lwt st =
    let pos = byte_pos_of_utf_pos ~cursor_pos:r.cursor_pos r.line in
    t.kernel.Kernel.complete ~pos r.line
  in
  let content = {
    matches=st.Kernel.completion_matches;
    cursor_start=utf_pos_of_byte_pos r.line ~pos:st.Kernel.completion_start;
    cursor_end=utf_pos_of_byte_pos r.line ~pos:st.Kernel.completion_end;
    cr_status="ok";
  } in
  send_shell t ~parent (M.Complete_reply content)

let is_complete_request t ~parent (r:is_complete_request): unit Lwt.t =
  let%lwt st = t.kernel.Kernel.is_complete r.icr_code in
  let content = match st with
    | Kernel.Is_complete ->
      {icr_status="complete"; icr_indent=""}
    | Kernel.Is_not_complete icr_indent ->
      {icr_status="incomplete"; icr_indent}
  in
  send_shell t ~parent (M.Is_complete_reply content)

let inspect_request (t:t) ~parent (r:Kernel.inspect_request) =
  let%lwt res =
    let pos = byte_pos_of_utf_pos ~cursor_pos:r.ir_cursor_pos r.ir_code in
    t.kernel.Kernel.inspect {r with ir_cursor_pos=pos}
  in
  let content = match res with
    | Ok r ->
      {
        ir_status = "ok";
        ir_found = Some r.Kernel.iro_found;
        ir_data = Some (dict_of_mime_bundle r.Kernel.iro_data);
        ir_metadata=None; (* TODO *)
        ir_ename =None; ir_evalue=None; ir_traceback=None;
      }
    | Error err_msg ->
      {
        ir_status = "error";
        ir_found=None; ir_data=None; ir_metadata=None;
        ir_ename = Some "error";
        ir_evalue = Some err_msg;
        ir_traceback = Some ["<inspect_request>"];
      }
  in
  send_shell t ~parent (M.Inspect_reply content)

let connect_request _socket _msg = () (* XXX deprecated *)

let history_request t ~parent x =
  let%lwt history = t.kernel.Kernel.history x in
  let content = {history} in
  send_shell t ~parent (M.History_reply content)

type run_result =
  | Run_stop
  | Run_restart

let run (t:t) : run_result Lwt.t =
  let () = Sys.catch_break true in
  Log.log "run on sockets...\n";
  let heartbeat =
    Sockets.heartbeat t.sockets >|= fun () -> Run_stop
  in
  let%lwt () =
    send_iopub t
      (Iopub_send_message (M.Status { execution_state = "starting" }))
  in
  (* initialize *)
  let%lwt () = t.kernel.Kernel.init () in
  let handle_message () =
    let open Sockets in
    let%lwt m = Lwt.pick
        [ M.recv t.sockets.shell;
          M.recv t.sockets.control;
        ] in
    Log.logf "received message `%s`, content `%s`\n"
      (M.msg_type_of_content m.M.content)
      (M.json_of_content m.M.content);
    begin match m.M.content with
      | M.Kernel_info_request ->
        within_status t
          ~f:(fun () -> kernel_info_request t ~parent:m)
      | M.Comm_info_request _r -> comm_info_request t ~parent:m
      | M.Execute_request x ->
        within_status t
          ~f:(fun () -> execute_request t ~parent:m x)
      | M.Connect_request ->
        Log.log "warning: received deprecated connect_request";
        connect_request t m; Lwt.return_unit
      | M.Inspect_request x ->
        within_status t ~f:(fun () -> inspect_request t ~parent:m x)
      | M.Complete_request x ->
        within_status t ~f:(fun () -> complete_request t ~parent:m x)
      | M.Is_complete_request x ->
        within_status t ~f:(fun () -> is_complete_request t ~parent:m x)
      | M.History_request x ->
        within_status t ~f:(fun () -> history_request t ~parent:m x)
      | M.Shutdown_request x -> shutdown_request t ~parent:m x

      (* messages we should not be getting *)
      | M.Connect_reply _ | M.Kernel_info_reply _
      | M.Shutdown_reply _ | M.Execute_reply _
      | M.Inspect_reply _ | M.Complete_reply _ | M.Is_complete_reply _
      | M.History_reply _ | M.Status _ | M.Execute_input _
      | M.Execute_result _ | M.Stream _ | M.Display_data _
      | M.Execute_error _ | M.Clear _ | M.Comm_info_reply ->
        handle_invalid_message ()

      | M.Comm_open -> Lwt.return_unit
    end
  in
  let rec run () =
    begin
      try%lwt
        handle_message() >|= fun _ -> Ok ()
      with
        | Sys.Break ->
          Log.log "Sys.Break\n";
          Lwt.return_ok ()
        | Restart ->
          Log.log "Restart\n";
          Lwt.return_error Run_restart
        | Exit ->
          Log.log "Exiting, as requested\n";
          Lwt.return_error Run_stop
    end >>= function
    | Ok () -> run()
    | Error e -> Lwt.return e
  in
  Lwt.pick [run (); heartbeat]
