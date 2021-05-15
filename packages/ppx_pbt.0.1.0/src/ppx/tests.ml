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

(*------ Parse properties ------*)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Parser.properties Lexer.token lexbuf_pps

open Ppxlib
open Error

let get_tested_fun_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_var { txt = str; _ } -> Some str
  | _ -> None

(* Extract fun name we want to test *)
let rec get_tested_fun_values_binding values_bindings =
  (* A structured item should contains only one pbt attribute *)
  List.map get_tested_fun_value_binding values_bindings |> List.hd

and get_tested_fun_expression_desc expr_desc =
  match expr_desc with
  | Pexp_let (_, values_binding, _) ->
      get_tested_fun_values_binding values_binding
  | Pexp_ident longident_loc -> get_tested_fun_longident_loc longident_loc
  | _ -> raise (CaseUnsupported "get_tested_fun_expression_desc")

and get_tested_fun_value_binding value_binding =
  match get_tested_fun_pattern value_binding.pvb_pat with
  (* In case of let f <pattern> = <expr>, the function name is located inside
     the value_binding.pattern *)
  | Some str -> str
  (* Otherwise, we look for the function name in the expression *)
  | None -> get_tested_fun_expression_desc value_binding.pvb_expr.pexp_desc

and get_tested_fun_longident_loc longident_loc =
  match longident_loc.txt with
  | Ldot (_, str) -> str
  | _ -> raise (CaseUnsupported "get_tested_fun_longident_loc")

(* Build_gens loc properties

   [gen1] -> gen1
   [gen1; gen2] -> (pair gen1 gen2)
   [gen1; gen2; gen3] -> (pair gen1 (pair gen2 gen3))
   ...
 *)
let build_gens loc (name, _args, gens) =
  let _ = Properties.check_gens loc name gens in
  let gens = Gens.replace_gens loc gens in
  let nested_gens = Gens.nest_generators gens in
  (Gens.nested_pairs_to_expr loc nested_gens, nested_gens)

(* Build_testing _ *)
let build_testing_fun loc nested_gens fun_name (name, args, _) =
  let _ = Properties.check_args loc name args in
  let (fun_pattern, gens) = Properties.pattern_from_gens loc nested_gens in
  let call_property =
    Properties.call_property loc fun_name (name, args, gens)
  in
  [%expr fun [%p fun_pattern] -> [%e call_property]]

(* Build_test loc fun_name properties

   <build_gens>
   <build_testing_fun> *)
let build_test loc fun_name properties =
  let (gens_exp, nested_gens) = build_gens loc properties in
  let property_exp = build_testing_fun loc nested_gens fun_name properties in
  (gens_exp, property_exp)

(* Build fun_name (name, args) :

   let test_<fun_name>_is_<name> = QCheck.Test.make ~name:<name> <build_test> *)
let build ~loc fun_name ((name, _, _) as properties) =
  let test_name = Format.sprintf "test_%s_is_%s" fun_name name in
  let test_name_var = Helpers.build_pattern_var loc test_name in
  let qcheck_name =
    Helpers.build_string loc @@ Format.sprintf "%s_is_%s" fun_name name
  in
  let (gens, test) = build_test loc fun_name properties in
  let qcheck_test =
    [%stri
      let [%p test_name_var] =
        QCheck.Test.make ~name:[%e qcheck_name] [%e gens] [%e test]]
  in

  (test_name, qcheck_test)

let exec_tests loc tests_names =
  let tests =
    Properties.args_to_expr loc tests_names
    |> List.map snd |> Helpers.build_list loc
  in
  [%stri let _ = QCheck_runner.run_tests ~verbose:true [%e tests]]

let replace_tests ~loc stri properties =
  let tests_generated =
    match stri.pstr_desc with
    | Pstr_value (_, values_bindings) ->
        let fun_name = get_tested_fun_values_binding values_bindings in
        let tests = List.map (build ~loc fun_name) properties in
        List.map snd tests @ [ exec_tests loc (List.map fst tests) ]
    (* TODO: better error management *)
    | _ -> assert false
  in
  stri :: tests_generated

let replace_pbt structure_item = function
  (* Structure item by construction can attach only one property *)
  | [ (pbt, loc) ] ->
      Payload.extract_pbt_from_payload pbt
      |> from_string
      |> replace_tests ~loc structure_item
  (* TODO: better error management *)
  | _ -> assert false
