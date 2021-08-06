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
module AH = Common.Ast_helpers
module E = AH.Expression
module P = AH.Pattern
module Info = Common.Helpers.Info
module Payload = Common.Payload
module Pairs = Common.Helpers.Pairs

let rec properties_to_test ~loc ~name ?sig_item properties =
  let (tests, names) =
    List.split @@ List.map (property_to_test ~loc ~name ?sig_item) properties
  in
  let loc = Location.none in
  let names = E.pexp_list ~loc (List.map (E.pexp_lident ~loc) names) in
  let add_runner = [%stri let () = Runner.add_tests [%e names]] in
  tests @ [ add_runner ]

(** [property_to_test loc name properties] create a test for a single property
    on the function called [name].
    Returns the test and its identifier as a string.

    The optional paramater [?sig_item] can be used to infer required generator
    using {{:https://github.com/vch9/ppx_deriving_qcheck}ppx_deriving_qcheck}. *)
and property_to_test ~loc ~name ?sig_item (property, args, gens) =
  let (pat_name, expr_name, test_name) = name_to_test ~loc name property in
  let (gens_expr, gens) = Gens.create_gens ~loc sig_item property gens in
  let tested_fun = pbt_to_test ~loc name property gens args in

  let test =
    [%stri
      let [%p pat_name] =
        QCheck.Test.make ~name:[%e expr_name] [%e gens_expr] [%e tested_fun]]
  in
  (test, test_name)

(** [name_to_test loc name property] builds and returns

    - An expression:
    {[ let () = Runner.add_test [name] ]}
    - A pattern:
    {[ let [name] = QCheck.Test.make .. ]}
    - A QCheck test name:
    {[ let .. = QCheck.Test.make ~name:[name] ]} *)
and name_to_test ~loc name property =
  let qcheck_name = Format.sprintf "%s_is_%s" name property in
  let test_name = Format.sprintf "test_%s" qcheck_name in
  let expr_name = E.pexp_string ~loc qcheck_name in
  let pat_name = P.ppat_var ~loc test_name in

  (pat_name, expr_name, test_name)

(** [pbt_to_test loc name propertyes gens args] creates the boolean function
    used in a QCheck test. *)
and pbt_to_test ~loc name property gens args =
  let (fun_pattern, gens) =
    Pairs.pattern_from_gens loc (fun x -> "arb_" ^ x) gens
  in
  let call = Properties.call_property loc name (property, args, gens) in
  [%expr fun [%p fun_pattern] -> [%e call]]
