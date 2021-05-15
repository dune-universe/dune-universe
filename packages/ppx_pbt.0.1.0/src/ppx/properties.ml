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

open Gens
open Ppxlib
open Error

type property_name = string

type arg = string

type gen = string

type property = property_name * arg list * arg list

type properties = property list

and t = properties

(* TODO: change to formatted version *)
let rec properties_to_str = function
  | [ p ] -> Format.sprintf "%s\n" (property_to_str p)
  | p1 :: p2 :: pps ->
      Format.sprintf
        "%s ;\n%s"
        (property_to_str p1)
        (properties_to_str (p2 :: pps))
  | [] -> assert false

and property_to_str (name, args, gens) =
  Format.sprintf "%s{%s}[%s]" name (args_to_str gens) (args_to_str args)

and args_to_str = function
  | [] -> ""
  | [ arg ] -> arg
  | arg1 :: arg2 :: args ->
      Format.sprintf "%s ; %s" arg1 (args_to_str (arg2 :: args))

(* Builtin properties contains two fields:
   expr <= Ast.expression calling the Pbt.Properties.f
   n_gens <= Number of generators required to call Pbt.Properties f
   n_args <= Number of arguments required to call Pbt.Properties f *)
type builtin_properties = { expr : expression; n_gens : int; n_args : int }

let builtin_properties loc x =
  [
    ( "commutative",
      { expr = [%expr Pbt.Properties.commutative]; n_gens = 2; n_args = 0 } );
    ( "associative",
      { expr = [%expr Pbt.Properties.associative]; n_gens = 3; n_args = 0 } );
    ( "neutral_left",
      { expr = [%expr Pbt.Properties.neutral_left]; n_gens = 1; n_args = 1 } );
    ( "neutral_right",
      { expr = [%expr Pbt.Properties.neutral_right]; n_gens = 1; n_args = 1 } );
    ( "neutrals",
      { expr = [%expr Pbt.Properties.neutrals]; n_gens = 1; n_args = 1 } );
    ("capped", { expr = [%expr Pbt.Properties.capped]; n_gens = 1; n_args = 1 });
    ( "floored_left",
      { expr = [%expr Pbt.Properties.floored_left]; n_gens = 1; n_args = 1 } );
    ("eq_res", { expr = [%expr Pbt.Properties.eq_res]; n_gens = 2; n_args = 1 });
    ( "absorb_left",
      { expr = [%expr Pbt.Properties.absorb_left]; n_gens = 1; n_args = 1 } );
    ( "absorb_right",
      { expr = [%expr Pbt.Properties.absorb_right]; n_gens = 1; n_args = 1 } );
    ( "absorbs",
      { expr = [%expr Pbt.Properties.absorbs]; n_gens = 1; n_args = 1 } );
  ]
  |> List.assoc_opt x

(* We check if the number of generator from the payload arguments is equal
   to the required number.

   [@@pbt {| property_name[gen0, gen1, .., genN] |} ]

   To each property_name is attached a number of required generators,
   see builtin_properties *)
let check_gens loc property_name gens =
  match builtin_properties loc property_name with
  | Some { n_gens = n; _ } ->
      let len = List.length gens in
      if n <> len then raise (PropertyGeneratorsMissing (property_name, n, len))
  | None ->
      Printf.printf
        "Ppx_pbt (Warning): %s is your local property, can not check generators\n"
        property_name

(* Same as check_gens with args *)
let check_args loc property_name args =
  match builtin_properties loc property_name with
  | Some { n_args = n; _ } ->
      let len = List.length args in
      if n <> len then raise (PropertyGeneratorsMissing (property_name, n, len))
  | None ->
      Printf.printf
        "Ppx_pbt (Warning): %s is your local property, can not check arguments\n"
        property_name

(* Applied arguments depends on the given generators

   example:

   (fun gen0 -> <property> <tested_fun> gen0)
   (fun (gen0, gen1) -> <property> <tested_fun> gen0 gen1)
   (fun (gen0, (gen1, gen2)) -> <property> <tested_fun> gen0 gen1 gen2) *)
let create_assoc_args gens =
  let id = ref 0 in
  let create_fresh_name i =
    let x = !i in
    i := !i + 1 ;
    "gen_" ^ string_of_int x
  in
  (* Replace_by_id replace the generators pattern by identifiers refering to the
     function pattern *)
  let rec replace_by_id = function
    | Pair (x, y) ->
        let x = replace_by_id x in
        let y = replace_by_id y in
        Pair (x, y)
    | Double _ ->
        let x = create_fresh_name id in
        let y = create_fresh_name id in
        Double (x, y)
    | Simple _ -> Simple (create_fresh_name id)
  in
  replace_by_id gens

(* Transform the nested_pairs from Generators into Pattern.tuple *)
let pattern_from_gens loc gens =
  let rec create_pattern loc = function
    | Pair (x, y) ->
        [%pat? ([%p create_pattern loc x], [%p create_pattern loc y])]
    | Double (x, y) ->
        let arg_x = Helpers.build_pattern_var loc x in
        let arg_y = Helpers.build_pattern_var loc y in
        Ppat_tuple [ arg_x; arg_y ] |> Helpers.build_pattern loc
    | Simple x -> Helpers.build_pattern_var loc x
  in
  let args = create_assoc_args gens in
  (* Pattern is returned with the assoc between generators and the identifier
     given in create_assoc_args *)
  (create_pattern loc args, args)

let args_to_expr loc args =
  let f x = (Nolabel, Helpers.build_ident loc x) in
  List.map f args

(* Build the call to the property intented to be tested

   (fun .. -> Pbt.Property.property_name gen0 gen1 ..) *)
let call_property loc fun_name (name, args, gens) =
  let args =
    fun_name :: args @ Gens.nested_pairs_to_list gens |> args_to_expr loc
  in
  match builtin_properties loc name with
  | Some { expr = fun_expr; _ } -> Helpers.build_apply loc fun_expr args
  | None -> Helpers.build_apply loc (Helpers.build_ident loc name) args
