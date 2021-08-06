(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Ppxlib

type md_expr = [ `Structure | `Functor ] [@@deriving show]

type path = [ `Psig_module of string | md_expr ] list [@@deriving show]

type psig_value = {
  name : string;
  path : path;
  properties : Properties.t;
  value : signature_item option;
      [@printer Format.pp_print_option Pprintast.signature_item]
}
[@@deriving show]

let get_path x = x.path

let get_properties x = x.properties

let get_name x = x.name

let get_value x = x.value

type t = {
  mutable file_name : string option;
  mutable psig_values : psig_value list;
}
[@@deriving show]

let _ = pp

let empty_env = { file_name = None; psig_values = [] }

let env = ref empty_env

let get_file_name () = Option.value ~default:"_none_" !env.file_name

let get_file_name_exn () = Option.get !env.file_name

let get_file_name_opt () = !env.file_name

let get_psig_values () = !env.psig_values

let env_with_psig_value x = !env.psig_values <- x :: !env.psig_values

let env_with_file_name x = !env.file_name <- Some x

let add_env ?(path = []) ?(properties = []) ?value name =
  env_with_psig_value { path; name; properties; value }

let reset_env () =
  !env.file_name <- None ;
  !env.psig_values <- []

let init_env ?file_name () =
  reset_env () ;
  !env.file_name <- file_name

let path_env file_name =
  Filename.remove_extension file_name
  |> Filename.basename
  |> Printf.sprintf "/tmp/%s.bytes"

let store_env () =
  match !env.file_name with
  | None -> failwith "Can not store the environment without the file name"
  | Some fn ->
      let path = path_env fn in
      let out_channel = open_out path in
      let () = Marshal.to_channel out_channel !env [] in
      close_out out_channel

let fetch_env file_name =
  try
    let path = path_env file_name in
    let in_channel = open_in path in
    let () = env := Marshal.from_channel in_channel in
    close_in in_channel
  with _ -> init_env ()

let pp fmt = pp fmt !env
