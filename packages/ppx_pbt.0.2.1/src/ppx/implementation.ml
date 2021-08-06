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

(** [get_attributes stri] extracts attributes from [stri] *)
let get_attributes stri =
  match stri.pstr_desc with
  | Pstr_value (_, xs) -> List.concat @@ List.map (fun x -> x.pvb_attributes) xs
  | Pstr_eval (_, xs) -> xs
  | _ -> []

let structure_item ~callback stri =
  let attributes = get_attributes stri in
  let properties = Helpers.get_properties attributes in
  let n_pbt = List.length properties in
  match stri with
  (* let f args = expr [@@pbt <properties>] *)
  | [%stri let [%p? f] = [%e? _body]] when n_pbt > 0 ->
      let name = Option.get @@ Helpers.extract_name_from_pattern f in
      let loc = stri.pstr_loc in

      let tests = Core__Tests.properties_to_test ~loc ~name properties in
      Common__Ast_helpers.Structure.str_include ~loc @@ stri :: tests
  | _ -> callback stri
