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
module Env = Core.Local_env

let interface file_name =
  let () = Env.fetch_env file_name in
  Filename.basename file_name = Env.get_file_name ()

(** [find_attributes code_path sig] does an in-depth course of a signature_item.

    It looks for [Psig_value] where there's an attribute "pbt" attached to it.
    Every occurences of signature item with the "pbt" attribute is stored in the
    local environment, in order to be use in {!inline_impl_tests}

    Once a recursive case is reached, we store the current path and go deeper. *)
let rec find_attributes x = find_attributes_signature_item [] x

and find_attributes_signature_item code_path sigi =
  match sigi.psig_desc with
  | Psig_value vd -> (
      let properties = Helpers.get_properties vd.pval_attributes in
      match properties with
      | [] -> ()
      | _ ->
          let path = List.rev code_path in
          let name = vd.pval_name.txt in
          let value = sigi in
          Env.add_env ~path ~properties ~value name)
  | Psig_module { pmd_name = { txt = Some name; _ }; pmd_type = mtd; _ } ->
      let code_path = `Psig_module name :: code_path in
      find_attributes_module_type code_path mtd
  | _ -> ()

and find_attributes_module_type (code_path : Env.path) mtd =
  match mtd.pmty_desc with
  | Pmty_signature sigs ->
      let code_path = `Structure :: code_path in
      List.iter (find_attributes_signature_item code_path) sigs
  | Pmty_functor (_, mtd) ->
      let code_path = `Functor :: code_path in
      find_attributes_module_type code_path mtd
  | _ -> failwith "TODO"

(** [find_and_replace does the actual in-depth course of structured_item according
    to the path found in a signature_item *)
let rec find_and_replace (path, properties, name, sigi) stri : structure_item =
  let loc = stri.pstr_loc in

  match (path, stri) with
  | ([], [%stri let [%p? pat] = [%e? _]])
  | ([], [%stri let rec [%p? pat] = [%e? _]]) -> (
      match Helpers.extract_name_from_pattern pat with
      | Some name' when name = name' ->
          let tests =
            Core__Tests.properties_to_test
              ~loc:stri.pstr_loc
              ~name
              ?sig_item:sigi
              properties
          in
          Common__Ast_helpers.Structure.str_include ~loc @@ stri :: tests
      | _ -> stri)
  | ( `Psig_module mod_name :: path,
      {
        pstr_desc =
          Pstr_module ({ pmb_name = { txt = Some mod_name'; _ }; _ } as md);
        _;
      } )
    when mod_name = mod_name' ->
      {
        stri with
        pstr_desc =
          Pstr_module
            {
              md with
              pmb_expr =
                find_and_replace_module_expr
                  (path, properties, name, sigi)
                  md.pmb_expr;
            };
      }
  | (_, _) -> stri

and find_and_replace_module_expr (path, properties, name, sigi) mdexpr :
    module_expr =
  match (path, mdexpr.pmod_desc) with
  | (`Functor :: path, Pmod_functor (func_param, md)) ->
      let md = find_and_replace_module_expr (path, properties, name, sigi) md in
      { mdexpr with pmod_desc = Pmod_functor (func_param, md) }
  | (`Structure :: path, Pmod_structure structure) ->
      let structure =
        List.map (find_and_replace (path, properties, name, sigi)) structure
      in
      { mdexpr with pmod_desc = Pmod_structure structure }
  | _ -> failwith "TODO"

(** [inline_impl_tests env str] replaces the according specification in mli with the
    actual implementation.

    For each Psig_value found in {!check_attributes} stored in the environment, we follow the
    path between the recursives structure_items until we eventually find the according
    Pstr_value using {!find_and_inline} *)
let inline_impl_tests structure : structure_item list =
  List.fold_left
    (fun structure psig_value ->
      let path = Env.get_path psig_value in
      let properties = Env.get_properties psig_value in
      let name = Env.get_name psig_value in
      let sig_item = Env.get_value psig_value in
      List.map (find_and_replace (path, properties, name, sig_item)) structure)
    structure
    (Env.get_psig_values ())

let intf file_name xs =
  let () = Env.init_env ~file_name () in
  let () = List.iter find_attributes xs in
  let () = Env.store_env () in
  xs
