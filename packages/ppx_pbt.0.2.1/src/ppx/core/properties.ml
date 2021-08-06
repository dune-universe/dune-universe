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
open Common.Helpers.Pairs
module Error = Common.Error
module AH = Common.Ast_helpers
module E = AH.Expression
module P = AH.Pattern

type property_name = string [@@deriving show]

type arg = string [@@deriving show]

type gen = string [@@deriving show]

type property = property_name * arg list * gen list [@@deriving show]

type properties = property list [@@deriving show]

and t = properties [@@deriving show]

let _check_help loc ppty xs ty =
  let (required, msg, f) =
    match ty with
    | `Args ->
        (Pbt.Properties.nb_of_args ppty, "arguments", Error.property_arg_missing)
    | `Gens ->
        ( Pbt.Properties.nb_of_gens ppty,
          "generators",
          Error.property_gen_missing )
  in

  match required with
  | Some n ->
      let len = List.length xs in
      if n <> len then f ~loc ~property:ppty ~required:n ~actual:len ()
  | None ->
      Printf.printf
        "Ppx_pbt (Warning): %s is your local property, can not check %s\n"
        ppty
        msg

let args_to_expr loc args =
  let f x = (Nolabel, E.pexp_lident ~loc x) in
  List.map f args

let call_property loc fun_name (name, args, gens) =
  let args = fun_name :: args @ nested_pairs_to_list gens |> args_to_expr loc in
  let f = Pbt.Properties.from_string ~loc name in
  E.pexp_apply ~loc ~f ~args ()
