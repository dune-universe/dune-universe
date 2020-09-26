(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

(* finally and with_dispose taken from Batteries *)
let finally = fun handler f x ->
  let res = try f x with exn -> handler (); raise exn in
  handler ();
  res

let with_dispose = fun dispose f x -> finally (fun () -> dispose x) f x

let parse = fun ch ->
  let lexbuf = Lexing.from_channel ch in
  try Prj_parser.prj Prj_lexer.token lexbuf
  with exn -> begin
    let tok = Lexing.lexeme lexbuf in
    Printf.eprintf "\nError while reading token:\"%s\"\n%!" tok;
    raise exn end

let read = fun file ->
  let ch = open_in file in
  with_dispose close_in parse ch
