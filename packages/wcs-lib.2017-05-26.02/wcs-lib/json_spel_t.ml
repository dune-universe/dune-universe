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

(** JSON with embedded expressions. *)


type json_spel = [
    `Assoc of (string * json_spel) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json_spel list
  | `Null
  | `Expr of Spel_t.expression
]

(** {6 Serialization/deserialization functions for atdgen} *)

let rec yojson_of_json_spel (j: json_spel) : Yojson.Basic.json =
  begin match j with
  | `Assoc l ->
      `Assoc (List.map (fun x -> (fst x, yojson_of_json_spel (snd x))) l)
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `List l -> `List (List.map yojson_of_json_spel l)
  | `Null -> `Null
  | `Expr e -> `String (Spel_print.to_text e)
  end

let rec json_spel_of_yojson (j: Yojson.Basic.json) : json_spel =
  begin match j with
  | `Assoc l ->
      `Assoc (List.map (fun x -> (fst x, json_spel_of_yojson (snd x))) l)
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `List l -> `List (List.map json_spel_of_yojson l)
  | `Null -> `Null
  | `String s -> `Expr (Spel_parse.quoted_expr_from_string s)
  (* This catches parse errors at the expression level *)
  end


let write_json_spel buff x =
  Yojson.Basic.write_json buff (yojson_of_json_spel x)

let read_json_spel state lexbuf =
  json_spel_of_yojson (Yojson.Basic.read_json state lexbuf)

let to_string x =
  Yojson.Basic.to_string (yojson_of_json_spel x)
