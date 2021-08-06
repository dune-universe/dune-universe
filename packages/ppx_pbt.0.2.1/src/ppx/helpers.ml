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

(** [pbt_name] constant name for attributes *)
let pbt_name = "pbt"

(** [extract_name_from_pattern pat] tries to extract the function name
    located in the pattern

    {[ let <pattern> = <expr> ]} *)
let extract_name_from_pattern pat : string option =
  match pat.ppat_desc with
  | Ppat_any -> None
  | Ppat_var { txt = x; _ } -> Some x
  | _ -> None

(** [filter_attributes expected attributes] filters [attributes] with name [expected] *)
let filter_attributes expected xs =
  List.filter (fun attr -> attr.attr_name.txt = expected) xs

(** [from_string properties] parse [properties] and returns a Properties.t *)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Core.Parser.properties Core.Lexer.token lexbuf_pps

(** [get_properties attributes] returns the list propertiy inside [attributes]

    Step 1: keep every attribute named {!pbt_name}
    Step 3: extract each attribute's payload, which must be a string constant
    Step 3: parse the properties
    Step 4: concat every properties into a single list

    Implicitly the function returns an empty list of properties if there is not
    properties attached on the attributes *)
let get_properties attributes =
  filter_attributes pbt_name attributes
  |> List.map Common.Payload.pbt_from_attribute
  |> List.map from_string |> List.concat
