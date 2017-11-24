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

let pretty_json_string s =
  (Yojson.Basic.pretty_to_string (Yojson.Basic.from_string s))

let before_default (msg_req: message_request) : message_request =
  msg_req

let after_default (msg_resp: message_response) : message_response =
  msg_resp

let user_input_default () =
  print_string "H: "; flush stdout;
  input_line stdin

let output_default txt =
  print_string "C: "; print_endline txt


let interpret
      ?(before=before_default)
      ?(after=after_default)
      (wcs_cred: credential)
  : (string -> message_request -> string * message_response * json option) =
  let rec interpret
            (ws_id: string)
            (req_msg: message_request)
    : string * message_response * json option =
    let req_msg = before req_msg in
    Log.debug "Wcs_bot"
      ("Request:\n"^
       (Wcs_pretty.message_request req_msg));
    let resp =
      Wcs_api.message wcs_cred ws_id req_msg
    in
    Log.debug "Wcs_bot"
      ("Response:\n"^
       (Wcs_pretty.message_response resp));
    let resp = after resp in
    let ctx = resp.msg_rsp_context in
    let output = resp.msg_rsp_output in
    begin match Context.take_actions ctx with
    | ctx, Some [ act ] ->
        let k =
          { act_name = ws_id;
            act_agent = "client";
            act_type_= "conversation";
            act_parameters = Json.assoc ["context", ctx];
            act_result_variable = act.act_result_variable; }
        in
        let act_ctx =
          begin match Json.get act.act_parameters "context" with
          | None -> `Null
          | Some ctx -> ctx
          end
        in
        let act_parameters =
          Json.set act.act_parameters "context"
            (Context.set_continuation act_ctx k)
        in
        let act = { act with act_parameters = act_parameters } in
        interpret_action act output
    | ctx, Some (_ :: _ :: _) ->
        assert false (* XXX TODO XXX *)
    | ctx, Some []
    | ctx, None ->
        let ctx, skip_user_input = Context.take_skip_user_input ctx in
        begin match Context.get_return ctx with
        | Some v ->
            begin match Context.get_continuation ctx with
            | Some k ->
                let k_txt =
                  if skip_user_input then
                    req_msg.msg_req_input.in_text
                  else
                    ""
                in
                let k_ctx =
                  begin match Json.get k.act_parameters "context" with
                  | Some ctx -> ctx
                  | None -> `Null
                  end
                in
                let k_ctx =
                  begin match k.act_result_variable with
                  | None -> k_ctx
                  | Some lbl ->
                      let prefix = String.sub lbl 0 8 in
                      let var = String.sub lbl 8 (String.length lbl - 8) in
                      assert (prefix = "context.");
                      Json.set k_ctx var v
                  end
                in
                let k_ctx, k_skip_user_input =
                  Context.take_skip_user_input k_ctx
                in
                if k_skip_user_input then
                  let k_parameters =
                    Json.assign
                      [ k.act_parameters;
                        Json.assoc ["text", Json.string k_txt];
                        Json.assoc ["context", k_ctx]; ]
                  in
                  let k = { k with act_parameters = k_parameters } in
                  interpret_action k output
                else
                  let k_resp =
                    { resp with msg_rsp_context = k_ctx }
                  in
                  (k.act_name, k_resp, Context.get_return k_ctx)
            | None ->
                (ws_id, resp, Some v)
            end
        | None ->
            if skip_user_input then
              interpret
                ws_id
                { req_msg with
                  msg_req_context = Some ctx;
                  msg_req_output = Some output;
                }
            else
              (ws_id, { resp with msg_rsp_context = ctx }, None)
        end
    end

  and interpret_action act output =
    begin match act.act_agent, act.act_type_ with
    | "client", "conversation" ->
        let ctx =
          begin match Json.get act.act_parameters "context" with
          | None -> `Null
          | Some ctx -> ctx
          end
        in
        let txt =
          begin match Json.get_string act.act_parameters "text" with
          | None -> ""
          | Some s -> s
          end
        in
        let req_msg =
          { msg_req_input = { in_text = txt };
            msg_req_alternate_intents = false;
            msg_req_context = Some ctx;
            msg_req_entities = None;
            msg_req_intents = None;
            msg_req_output = Some output; }
        in
        interpret act.act_name req_msg
    | _ -> assert false
    end
  in
  interpret

let exec
      ?(before=before_default)
      ?(after=after_default)
      ?(user_input=user_input_default)
      ?(output=output_default)
      (wcs_cred: credential)
      (workspace_id: string)
      (ctx_init: json)
      (txt_init: string)
  : json =
  let interpret =
    interpret ~before ~after wcs_cred
  in
  let rec loop ws_id ctx txt =
    let req =
      { msg_req_input = { in_text = txt };
        msg_req_alternate_intents = false;
        msg_req_context = Some ctx;
        msg_req_entities = None;
        msg_req_intents = None;
        msg_req_output = None; }
    in
    let ws_id, rsp, return = interpret ws_id req in
    List.iter output rsp.msg_rsp_output.out_text;
    begin match return with
    | Some v -> v
    | None ->
        let txt = user_input () in
        let ctx = rsp.msg_rsp_context in
        loop ws_id ctx txt
    end
  in
  loop workspace_id ctx_init txt_init

let get_credential file_name_opt =
  begin try
    let file_name =
      begin match file_name_opt with
      | Some file_name -> file_name
      | None -> Sys.getenv "WCS_CRED"
      end
    in
    Json.read_json_file Wcs_j.read_credential file_name
  with
  | Not_found ->
      Log.error "Wcs_bot" None ("no credential file")
  | exn ->
      Log.error "Wcs_bot" None (Printexc.to_string exn)
  end
