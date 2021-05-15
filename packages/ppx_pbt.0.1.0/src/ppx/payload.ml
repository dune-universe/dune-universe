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
open Error

let rec extract_pbt_from_payload = function
  | PStr structure -> extract_pbt_from_structure structure
  | _ -> raise (CaseUnsupported "extract_pbt_from_payload")

and extract_pbt_from_structure_item structure_item =
  match structure_item.pstr_desc with
  | Pstr_eval (expr, _) -> extract_pbt_from_expression expr
  | _ -> raise (CaseUnsupported "extract_pbt_from_structure_item")

and extract_pbt_from_structure structure =
  List.map extract_pbt_from_structure_item structure |> List.hd

and extract_pbt_from_expression expression =
  match expression.pexp_desc with
  | Pexp_constant constant -> extract_pbt_from_constant constant
  | _ -> raise (CaseUnsupported "extract_pbt_from_expression")

and extract_pbt_from_constant = function
  | Pconst_string (str, _, _) -> str
  | _ -> raise (CaseUnsupported "extract_pbt_from_constant")
