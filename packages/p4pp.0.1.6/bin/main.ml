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
open Command.Let_syntax
open P4pp

exception ParsingError of string

let go verbose include_dirs defines p4_file =
  let env = Eval.empty p4_file include_dirs defines in
  let str,_ = Eval.FileSystem.(preprocess env p4_file (load p4_file)) in 
  Printf.printf "%s" str

let define str =
  try 
    match String.split_on_chars ~on:['='] str with
    | [x] -> (x,"")
    | [x;y] -> (x,y)
    | _ -> failwith "Error"
  with  _ -> 
    failwith ("Error: malformed command-line argument '-D " ^ str ^ "'")
         
let command =
  Command.basic ~summary:"p4pp: P4 preprocessor"
  [%map_open 
     let verbose = flag "-verbose" no_arg ~doc:"Verbose mode"
     and includes = flag "-I" (listed string) ~doc:"<dir> Add directory to include search path"
     and defines = flag "-D" (listed string) ~doc:"<macro>=<value> Define macro"
     and p4_file = anon ("p4file" %: string) in
     (fun () -> 
       let defines = List.map defines ~f:define in
       go verbose includes defines p4_file)
  ]

let () =
  Format.printf "@[";
  Command.run ~version:"0.1.5" command;
  Format.printf "@]"
