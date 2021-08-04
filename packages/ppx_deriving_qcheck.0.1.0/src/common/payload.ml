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
module E = Error

let rec pbt_from_attribute x =
  match x.attr_payload with
  | PStr structure -> pbt_from_structure structure
  | _ ->
      E.case_unsupported
        ~loc:x.attr_loc
        ~case:"Common.Attribute.pbt_from_payload"
        ()

and pbt_from_structure_item stri =
  match stri.pstr_desc with
  | Pstr_eval (expr, _) -> pbt_from_expression expr
  | _ ->
      E.case_unsupported
        ~loc:stri.pstr_loc
        ~case:"Common.Attribute.pbt_from_structure_item"
        ()

and pbt_from_structure structure =
  (* TODO: This function should be property based tested,
     forall structure : List.length (pbt_from_structure structure) = 1 *)
  List.map pbt_from_structure_item structure |> List.hd

and pbt_from_expression expression =
  match expression.pexp_desc with
  | Pexp_constant constant -> pbt_from_constant constant
  | _ ->
      E.case_unsupported
        ~loc:expression.pexp_loc
        ~case:"Common.Attribute.pbt_from_expression"
        ()

and pbt_from_constant = function
  | Pconst_string (str, _, _) -> str
  | _ -> E.case_unsupported ~case:"Common.Attribute.pbt_from_constant" ()
