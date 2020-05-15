(* Copyright 2019-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open Core
open P4pp

exception ParsingError of string

let preprocess verbose include_dirs defines p4_file () =
  let buf = Buffer.create 101 in
  let env = Eval.{ file = p4_file; defines } in
  ignore (Eval.preprocess_file include_dirs env buf p4_file);
  Format.print_string (Buffer.contents buf)

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-verbose" no_arg ~doc:"Verbose mode"
    +> flag "-I" (listed string) ~doc:"<dir> Add directory to include search path"
    +> flag "-D" (listed string) ~doc:"<macro> Define macro"
    +> anon ("p4file" %:string) in
  Command.basic_spec
    ~summary:"p4pp: P4 preprocessor"
    spec
    (fun verbose includes defines file ->
      let defines = List.map defines ~f:(fun d -> (d,Int64.zero)) in
      preprocess verbose includes defines file)

let () =
  Format.printf "@[";
  Command.run ~version:"0.1.1" command;
  Format.printf "@]"
