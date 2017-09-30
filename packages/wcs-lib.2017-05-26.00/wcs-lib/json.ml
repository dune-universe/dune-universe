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

let read_json_file reader f =
  begin try
    let lexstate = Yojson.init_lexer ~fname:f () in
    let ch = open_in f in
    let lexbuf = Lexing.from_channel ch in
    let json = reader lexstate lexbuf in
    close_in ch;
    json
  with
  | Yojson.Json_error err ->
      Log.error "Json_util" None
        ("Unable to parse file "^f^": "^err)
  | exn ->
      Log.error "Json_util" None
        ("Unable to read file "^f^": "^(Printexc.to_string exn))
  end


(** {6. Utils} *)

let null : json = `Null

let set (ctx: json) (lbl: string) (v: json) : json =
  begin match ctx with
  | `Null -> `Assoc [ lbl, v ]
  | `Assoc l -> `Assoc ((lbl, v) :: (List.remove_assoc lbl l))
  | _ ->
      Log.error "Json"
        (Some ctx)
        "Unable to add a property to a non-object value"
  end

let take (ctx: json) (lbl: string) : json * json option =
  begin match ctx with
  | `Assoc l ->
      begin try
        let v = List.assoc lbl l in
        `Assoc (List.remove_assoc lbl l), Some v
      with Not_found ->
        ctx, None
      end
  | _ -> ctx, None
  end

let get (ctx: json) (lbl: string) : json option =
  begin try
    begin match Yojson.Basic.Util.member lbl ctx with
    | `Null -> None
    | x -> Some x
    end
  with _ ->
    None
  end


(** {6. skip_user_input} *)

let set_skip_user_input (ctx: json) (b: bool) : json =
  set ctx "skip_user_input" (`Bool b)

let take_skip_user_input (ctx: json) : json * bool =
  begin match take ctx "skip_user_input" with
  | ctx, Some (`Bool b) -> ctx, b
  | _ -> ctx, false
  end


(** {6. Actions} *)

let yojson_of_action (act : action) : json =
  Yojson.Basic.from_string (Wcs_j.string_of_action act)

let action_of_yojson (act : json) : action =
  Wcs_j.action_of_string (Yojson.Basic.to_string act)

let set_actions ctx (acts: action list) : json =
  let js_acts = List.map yojson_of_action acts in
  set ctx "actions" (`List js_acts)

let take_actions (ctx: json) : json * action list option =
  begin match take ctx "actions" with
  | ctx', Some (`List acts) ->
      begin try
        ctx', Some (List.map action_of_yojson acts)
      with _ ->
        Log.warning "Json"
          (Format.sprintf "illed formed actions:\n%s@."
             (Yojson.Basic.pretty_to_string (`List acts)));
        ctx, None
      end
  | _, Some o ->
      Log.warning "Json"
        (Format.sprintf "illed formed actions:\n%s@."
           (Yojson.Basic.pretty_to_string o));
      ctx, None
  | _, None ->
      ctx, None
  end

let push_action (ctx: json) (act: action) : json =
  begin match take_actions ctx with
  | ctx, None ->
      set_actions ctx [ act ]
  | ctx, Some acts ->
      set_actions ctx (acts @ [ act ])
  end

let pop_action (ctx: json) : json * action option =
  begin match take_actions ctx with
  | ctx', Some (act :: acts) ->
      set_actions ctx' acts, Some act
  | _ -> ctx, None
  end

(** {6. Continuation} *)

let set_continuation (ctx: json) (k: action) : json =
  set ctx "continuation" (yojson_of_action k)

let take_continuation (ctx: json) : json * action option =
  begin match take ctx "continuation" with
  | ctx', Some act ->
      begin try
        ctx', Some (action_of_yojson act)
      with _ ->
        Log.warning "Json"
          (Format.sprintf "illed formed continuation:\n%s@."
             (Yojson.Basic.pretty_to_string act));
        ctx, None
      end
  | _ -> ctx, None
  end

let get_continuation (ctx: json) : action option =
  let _, act = take_continuation ctx in
  act


(** {6. Return} *)

let set_return (ctx: json) (x: json) : json =
  set ctx "return" x

let get_return (ctx: json) : json option =
  get ctx "return"

(** {6. Bool} *)

let set_bool (ctx: json) (lbl: string) (b: bool) : json =
  set ctx lbl (`Bool b)

let get_bool (ctx: json) (lbl: string) : bool option =
  begin match Yojson.Basic.Util.member lbl ctx with
  | `Bool b -> Some b
  | _ -> None
  end

(** {6. String} *)

let set_string (ctx: json) (lbl: string) (s: string) : json =
  set ctx lbl (`String s)

let get_string (ctx: json) (lbl: string) : string option =
  begin match get ctx lbl with
  | Some (`String s) -> Some s
  | _ -> None
  end

let take_string (ctx: json) (lbl: string) : json * string option =
  begin match take ctx lbl with
  | ctx, Some (`String s) -> ctx, Some s
  | ctx, _ -> ctx, None
  end

