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

(* ------------------------------------------------ *)
(* ------------ Print type_declaration ------------ *)
(* ------------------------------------------------ *)

let list_to_str ?(sep = " ; ") ?(left = "[") ?(right = "]") f l =
  let rec aux = function
    | [] -> ""
    | [ x ] -> f x
    | x :: xs -> f x ^ sep ^ aux xs
  in
  Printf.sprintf "%s %s %s" left (aux l) right

let rec type_decl_to_str td =
  Printf.sprintf
    {|
{
   ptype_name : %s;
   ptype_params : %s;
   ptype_cstrs : %s;
   ptype_kind : %s;
   ptype_private : %s;
   ptype_manifest : %s;
   ptype_attributes : %s;
   ptype_loc : %s
}
   |}
    td.ptype_name.txt
    (type_params_to_str td.ptype_params)
    (type_cstrs_to_str td.ptype_cstrs)
    (type_kind_to_str td.ptype_kind)
    (private_to_str td.ptype_private)
    (manifest_to_str td.ptype_manifest)
    (attributes_to_str td.ptype_attributes)
    "loc not printed"

and type_cstr_to_str (x, y, _) =
  Printf.sprintf "(%s, %s, _)" (core_type_to_str x) (core_type_to_str y)

and type_cstrs_to_str cstrs = list_to_str type_cstr_to_str cstrs

and type_param_to_str (ct, (variance, injectivity)) =
  Printf.sprintf
    "(%s, (%s, %s))"
    (core_type_to_str ct)
    (variance_to_str variance)
    (injectivity_to_str injectivity)

and variance_to_str = function
  | Covariant -> "covariant"
  | Contravariant -> "contravariant"
  | NoVariance -> "noVariance"

and injectivity_to_str = function
  | Injective -> "injective"
  | NoInjectivity -> "noInjectivity"

and type_params_to_str params = list_to_str type_param_to_str params

and core_type_to_str ct =
  match ct.ptyp_desc with
  | Ptyp_constr (id, cts) ->
      Printf.sprintf
        "K (%s, %s)"
        (longident_to_str id.txt)
        (core_types_to_str cts)
  | Ptyp_poly (strs, ct) ->
      Printf.sprintf
        "%s (%s)"
        (list_to_str (fun s -> s.txt) strs)
        (core_type_to_str ct)
  | Ptyp_any -> "any"
  | Ptyp_var s -> s
  | Ptyp_tuple cts ->
      list_to_str ~left:"(" ~sep:" , " ~right:")" core_type_to_str cts
  | Ptyp_arrow (x, left, right) ->
      let x' = arg_label_to_str x in
      let left' = core_type_to_str left in
      let right' = core_type_to_str right in
      Printf.sprintf "%s (%s -> %s)" x' left' right'
  | _ -> "TODO else core_type"

and core_types_to_str cts = list_to_str core_type_to_str cts

and arg_label_to_str = function
  | Nolabel -> "_"
  | Labelled s -> "~" ^ s
  | Optional s -> "?" ^ s

and type_kind_to_str = function
  | Ptype_abstract -> "abstract"
  (* TODO useful to print here *)
  | Ptype_variant constrs -> constr_declarations_to_str constrs
  | Ptype_record labels ->
      list_to_str ~left:"{" ~right:"}" label_declaration_to_str labels
  | Ptype_open -> "open"

and label_declaration_to_str ld =
  let name = ld.pld_name.txt in
  let mut =
    match ld.pld_mutable with Mutable -> "mutable" | Immutable -> "immutable"
  in
  let ty = core_type_to_str ld.pld_type in
  Printf.sprintf "(%s %s : %s)" mut name ty

and constr_declarations_to_str cstrs =
  list_to_str ~sep:"\n" constr_declaration_to_str cstrs

and constr_declaration_to_str cd =
  Printf.sprintf
    "{ pcd_name:%s; pcd_args:%s; pcd_res:%s; _ }"
    cd.pcd_name.txt
    (constr_args_to_str cd.pcd_args)
    (Option.fold ~none:"None" ~some:core_type_to_str cd.pcd_res)

and constr_args_to_str = function
  (* TODO really print if necessary *)
  | Pcstr_tuple cts -> core_types_to_str cts
  | Pcstr_record _ -> "Pcstr_record"

and attribute_to_str attr =
  Printf.sprintf "{attr_name : %s; _; _}" attr.attr_name.txt

and attributes_to_str attrs = list_to_str attribute_to_str attrs

and manifest_to_str ct = Option.fold ~none:"None" ~some:core_type_to_str ct

and longident_to_str = function
  | Lident s -> s
  | Ldot (lg, s) -> Printf.sprintf "%s.%s" (longident_to_str lg) s
  | Lapply (lg1, lg2) ->
      Printf.sprintf "%s %s" (longident_to_str lg1) (longident_to_str lg2)

and private_to_str = function Private -> "private" | Public -> "public"

let print_type_decl td = Printf.printf "%s\n" (type_decl_to_str td)
