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

let replace_gens ~loc gens =
  List.map
    (fun x ->
      let expr =
        Arbitrary__Types_helper.Primitive.from_string
          ~loc
          ~recursives_types:[]
          ~mutual_types:[]
          x
      in
      match expr.pexp_desc with
      (* That's a dirty hack to use from_string from ppx_deriving_qcheck and
         still use arbitrary from the local scope if it does not find an arbitrary
         in QCheck. *)
      | Pexp_ident { txt = Lident s; _ } ->
          let s = String.sub s 4 (String.length s - 4) in
          Common__Ast_helpers.Expression.pexp_lident ~loc s
      | _ -> expr)
    gens

(** [from_core_type_opt] is a try/catch version of
    {!Ppx_deriving_qcheck.Arbitrary.from_core_type}.

    The goal is to derive basic types from a signature_item, every
    exception raised from ppx_deriving_qcheck will be translated to None. *)
let from_core_type_opt ~loc x =
  let env = Arbitrary.env "_none_" in
  try Some (Arbitrary.from_core_type ~loc ~env x) with _ -> None

let rec arrow_to_list (x : core_type) : core_type list =
  match x.ptyp_desc with
  | Ptyp_arrow (_, left, right) ->
      let acc = arrow_to_list right in
      left :: acc
  | _ -> []

let nb_of_gens sigi =
  Option.fold
    ~none:None
    ~some:(fun x ->
      match x.psig_desc with
      | Psig_value { pval_type = ct; _ } ->
          arrow_to_list ct |> List.length |> Option.some
      | _ -> None)
    sigi

let takes_n l1 l2 n =
  let rec aux l1 l2 n =
    assert (n >= 0) ;
    match (n, l1, l2) with
    | (0, _, _) -> []
    | (n, x :: l1, _ :: l2) | (n, x :: l1, l2) -> x :: aux l1 l2 (n - 1)
    | (n, [], x :: l2) -> Option.get x :: aux l1 l2 (n - 1)
    | _ -> raise (Invalid_argument "")
  in
  aux l1 l2 n

let infer_gens_from_sig ~loc sig_item =
  match sig_item.psig_desc with
  | Psig_value { pval_type = ct; _ } ->
      arrow_to_list ct |> List.map (from_core_type_opt ~loc)
  | _ -> []

let create_gens ~loc sig_item property gens =
  let given_gens = replace_gens ~loc gens in
  let infered_gens =
    Option.fold ~none:[] ~some:(infer_gens_from_sig ~loc) sig_item
  in

  let gens =
    match (Pbt.Properties.nb_of_gens property, nb_of_gens sig_item) with
    | (None, None) -> given_gens
    | (Some n, _) | (_, Some n) -> (
        try takes_n given_gens infered_gens n
        with Invalid_argument _ ->
          let msg = Printf.sprintf "%s requires %d generators" property n in
          Common.Error.location_error ~loc ~msg ())
  in
  let nested_gens = Common__Helpers.Pairs.nest_generators gens in

  (Common__Helpers.Pairs.nested_pairs_to_expr loc nested_gens, nested_gens)
