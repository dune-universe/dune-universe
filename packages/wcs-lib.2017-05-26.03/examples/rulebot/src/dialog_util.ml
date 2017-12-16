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
open Json_t
open Deriving_intf

let (>>=) x f =
  match x with Ok x -> f x | (Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Ok (List.rev acc)

type 'a error_or = ('a, string) result

type workspace_ids =
  {
    ws_dispatch_id: string;
    ws_when_id: string;
    ws_cond_id: string;
    ws_cond_continue_id: string;
    ws_then_id: string;
    ws_expr_id: string;
    ws_actn_id: string;
    ws_accept_id: string;}
type 'a dispatch =
  {
    dsp_replace: bool;
    dsp_abort: bool;
    dsp_number: 'a option;
    dsp_when: bool;
    dsp_cond: bool;
    dsp_then: bool;}
let rec dispatch_to_yojson :
  'a . ('a -> Yojson.Safe.json) -> 'a dispatch -> Yojson.Safe.json=
  fun poly_a  ->
    ((
      fun x  ->
        let fields = [] in
        let fields = ("dsp_then", ((fun x  -> `Bool x) x.dsp_then)) ::
                     fields in
        let fields = ("dsp_cond", ((fun x  -> `Bool x) x.dsp_cond)) ::
                     fields in
        let fields = ("dsp_when", ((fun x  -> `Bool x) x.dsp_when)) ::
                     fields in
        let fields =
          ("dsp_number",
           ((function
            | None  -> `Null
            | Some x -> (poly_a : _ -> Yojson.Safe.json) x) x.dsp_number))
          :: fields in
        let fields = ("dsp_abort", ((fun x  -> `Bool x) x.dsp_abort)) ::
                     fields in
        let fields = ("dsp_replace", ((fun x  -> `Bool x) x.dsp_replace))
                     :: fields in
        `Assoc fields)[@ocaml.warning "-A"])
and dispatch_of_yojson :
  'a .
    (Yojson.Safe.json -> 'a error_or) ->
  Yojson.Safe.json -> 'a dispatch error_or=
  fun poly_a  ->
    ((
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4,arg5) as _state) =
            match xs with
            | ("dsp_replace",x)::xs ->
                loop xs
                  (((function
                   | `Bool x -> Ok x
                   | _ -> Error "Dialog_util.dispatch.dsp_replace")
                      x), arg1, arg2, arg3, arg4, arg5)
            | ("dsp_abort",x)::xs ->
                loop xs
                  (arg0,
                   ((function
                    | `Bool x -> Ok x
                    | _ -> Error "Dialog_util.dispatch.dsp_abort")
                      x), arg2, arg3, arg4, arg5)
            | ("dsp_number",x)::xs ->
                loop xs
                  (arg0, arg1,
                   ((function
                    | `Null -> Ok None
                    | x ->
                        ((poly_a : Yojson.Safe.json -> _ error_or) x) >>=
                        ((fun x  -> Ok (Some x)))) x), arg3,
                   arg4, arg5)
            | ("dsp_when",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                   ((function
                    | `Bool x -> Ok x
                    | _ -> Error "Dialog_util.dispatch.dsp_when")
                      x), arg4, arg5)
            | ("dsp_cond",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                   ((function
                    | `Bool x -> Ok x
                    | _ -> Error "Dialog_util.dispatch.dsp_cond")
                      x), arg5)
            | ("dsp_then",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3, arg4,
                   ((function
                    | `Bool x -> Ok x
                    | _ -> Error "Dialog_util.dispatch.dsp_then")
                      x))
            | [] ->
                arg5 >>=
                ((fun arg5  ->
                   arg4 >>=
                   (fun arg4  ->
                      arg3 >>=
                      (fun arg3  ->
                         arg2 >>=
                         (fun arg2  ->
                            arg1 >>=
                            (fun arg1  ->
                               arg0 >>=
                               (fun arg0  ->
                                  Ok
                                    {
                                      dsp_replace = arg0;
                                      dsp_abort = arg1;
                                      dsp_number = arg2;
                                      dsp_when = arg3;
                                      dsp_cond = arg4;
                                      dsp_then = arg5
                                    })))))))
            | _::xs -> Error "Dialog_util.dispatch" in
          loop xs
            ((Error "Dialog_util.dispatch.dsp_replace"),
             (Error "Dialog_util.dispatch.dsp_abort"),
             (Error "Dialog_util.dispatch.dsp_number"),
             (Error "Dialog_util.dispatch.dsp_when"),
             (Error "Dialog_util.dispatch.dsp_cond"),
             (Error "Dialog_util.dispatch.dsp_then"))
      | _ -> Error "Dialog_util.dispatch")[@ocaml.warning "-A"])
let int_dispatch_of_yojson =
  dispatch_of_yojson
    (fun json  ->
       match json with
       | `Int n -> Ok n
       | _ ->
           Error
             ("int_dispatch_of_yojson: " ^
              (Yojson.Safe.pretty_to_string json)))
let string_dispatch_to_yojson = dispatch_to_yojson (fun x  -> `String x)
let bypass_expr input =
  try
    let regexp = Str.regexp "`\\(.*\\)`" in
    let _ = Str.search_forward regexp input 0 in
    let quoted_string = Str.matched_group 1 input in
    try Some (false, (Parser_util.parse_cnl_expr_from_string quoted_string))
    with | e -> (Io_util.print_berl_error quoted_string; None)
  with | Not_found  -> None
let bypass_empty input = None
[@@@ocaml.text " "]
let match_string s =
  try
    try
      let regexp = Str.regexp "\"\\(.*\\)\"" in
      let _ = Str.search_forward regexp s 0 in
      let string = Str.matched_group 1 s in
      let s_minus_string = Str.global_replace regexp "" s in
      Some (s_minus_string, string)
    with
    | Not_found  ->
        let regexp = Str.regexp "'\\(.*\\)'" in
        let _ = Str.search_forward regexp s 0 in
        let string = Str.matched_group 1 s in
        let s_minus_string = Str.global_replace regexp "" s in
        Some (s_minus_string, string)
  with | Not_found  -> None[@@ocaml.text " "]
let debug_message req resp =
  Format.eprintf "request:@\n%s@\n" (Wcs_j.string_of_message_request req);
  Format.eprintf "response:@\n%s@." (Wcs_j.string_of_message_response resp)
[@@ocaml.doc " Debug functions "]
