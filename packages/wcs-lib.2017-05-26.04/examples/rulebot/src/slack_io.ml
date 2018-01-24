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

let launch cmd =
  let in_ch, out_ch = Unix.open_process cmd in
  let stream = Yojson.Basic.stream_from_channel in_ch in
  let get_input () =
    begin try
      let o = Stream.next stream in
      o |>
      Yojson.Basic.Util.member "text" |>
      Yojson.Basic.Util.to_string
    with e ->
      Format.eprintf "%s@." (Printexc.to_string e);
      ""
    end
  in
  let print_output rule_opt msg =
    let r =
      begin match rule_opt with
      | Some rule ->
          Printf.sprintf "```\n%s\n```\n"
            (Cnl_print.cnl_print_rule_top rule)
      | None -> ""
      end
    in
    let text = r ^ msg in
    let o = Yojson.Basic.to_string (`Assoc [("text", `String text)]) in
    print_endline o
  in
  (get_input, print_output)
