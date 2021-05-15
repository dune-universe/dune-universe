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

(*------ Find attributes in Ast.structure_item ------*)

let attribute_name = "pbt"

(* Meant for testing, if ignore is true -> attributes are ignored *)
let ignore = ref false

(* get_stri_pbt recursively find attributes in structured items *)
let rec get_stri_pbt stri =
  match stri.pstr_desc with
  | Pstr_eval (_, attributes) -> get_attributes_pbt attributes
  | Pstr_value (_, values_binding) -> get_values_binding_pbt values_binding
  | Pstr_module module_binding ->
      let pmb_expr = module_binding.pmb_expr in
      get_module_expr_pbt pmb_expr
  (* Default case: attributes are not yet to be found in others structured items *)
  | _ -> []

and get_structure_pbt structures =
  List.fold_left
    (fun acc structure -> get_stri_pbt structure @ acc)
    []
    structures

and get_module_expr_pbt module_expr =
  match module_expr.pmod_desc with
  | Pmod_structure structure -> get_structure_pbt structure
  | _ -> []

and get_values_binding_pbt vs_binds =
  List.fold_left
    (fun acc value_binding ->
      get_attributes_pbt value_binding.pvb_attributes @ acc)
    []
    vs_binds

and get_attributes_pbt attrs =
  List.filter_map
    (fun attribute ->
      if String.equal attribute.attr_name.txt attribute_name then
        Some (attribute.attr_payload, attribute.attr_loc)
      else None)
    attrs

let structure_item_contains_pbt stri = get_stri_pbt stri <> []

(*------ Replace structure item when attached with pbt attribute ------*)
let rec replace_structure_item stri : structure_item list =
  match stri.pstr_desc with
  (* -- Recursives cases -- *)
  (* module <Name> = struct .. end *)
  | Pstr_module module_binding ->
      [
        {
          stri with
          pstr_desc = Pstr_module (replace_module_binding module_binding);
        };
      ]
  (* -- Structures items where ppx_pbt is accepted -- *)
  (* let <fname> <args> = <expr> *)
  | Pstr_value _ ->
      let attributes = get_stri_pbt stri in
      if attributes <> [] then Tests.replace_pbt stri (get_stri_pbt stri)
      else [ stri ]
  | _ -> [ stri ]

and replace_structure structure =
  List.map replace_structure_item structure |> List.concat

and replace_module_binding module_binding =
  { module_binding with pmb_expr = replace_module_expr module_binding.pmb_expr }

and replace_module_expr module_expr =
  match module_expr.pmod_desc with
  | Pmod_structure structure ->
      {
        module_expr with
        pmod_desc = Pmod_structure (replace_structure structure);
      }
  | _ -> module_expr

let expand struct_item =
  try
    if (not !ignore) && structure_item_contains_pbt struct_item then
      replace_structure_item struct_item
    else [ struct_item ]
  with e ->
    Error.print_exception e ;
    raise InternalError

let impl xs = xs |> List.map expand |> List.concat

let () = Driver.register_transformation ~impl "ppx_pbt"
