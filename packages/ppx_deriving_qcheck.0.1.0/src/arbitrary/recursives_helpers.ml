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
module Error = Common.Error
module PP = Common.Pp

type ty = string

let rec is_recursive ~loc ~ty = function
  | Ptype_variant cstrs ->
      List.exists (is_recursive_constructor_declaration ~loc ty) cstrs
  | Ptype_record xs -> is_recursive_label_declarations ~loc ~ty xs
  | _ -> false

and is_recursive_constructor_declaration ~loc ty cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_recursive_core_type ~loc ~ty) cts
  | Pcstr_record xs -> is_recursive_label_declarations ~loc ~ty xs

and is_recursive_label_declarations ~loc ~ty xs =
  let labels =
    List.filter_map
      (fun x ->
        let loc = x.pld_type.ptyp_loc in
        match x.pld_type.ptyp_desc with
        | Ptyp_var s -> if s = ty then Some loc else None
        | Ptyp_constr (lg, _) ->
            let s = PP.longident_to_str lg.txt in
            if s = ty then Some loc else None
        | _ -> None)
      xs
  in
  match labels with
  | [] -> false
  | _ ->
      Error.location_error
        ~loc
        ~msg:"ppx_pbt does not supports recursive record"
        ()

and is_recursive_core_type ~loc ~ty ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = lg; _ }, cts) ->
      PP.longident_to_str lg = ty
      || List.exists (is_recursive_core_type ~loc ~ty) cts
  | Ptyp_variant (rws, _, _) -> is_recursive_row_fields ~loc ty rws
  | _ -> false

and is_recursive_row_field ~loc ty rw =
  match rw.prf_desc with
  | Rinherit ct -> is_recursive_core_type ~loc ~ty ct
  | Rtag (_, _, cts) -> List.exists (is_recursive_core_type ~loc ~ty) cts

and is_recursive_row_fields ~loc ty rws =
  List.exists (is_recursive_row_field ~loc ty) rws

let is_recursive_type_declaration ?loc td =
  let loc = Option.value ~default:Location.none loc in
  let ty = td.ptype_name.txt in
  is_recursive ~loc ~ty td.ptype_kind
  || Option.fold
       ~none:false
       ~some:(fun ct -> is_recursive_core_type ~loc ~ty ct)
       td.ptype_manifest

let get_recursives_type_declarations ?loc tds =
  List.filter_map
    (fun td ->
      if is_recursive_type_declaration ?loc td then Some td.ptype_name.txt
      else None)
    tds
