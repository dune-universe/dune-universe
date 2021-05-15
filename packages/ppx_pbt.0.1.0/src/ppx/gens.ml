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

(* Exhaustive list of builtin generators
   If the function returns None -> the generator must be provided by the user *)
let builtin_generators loc x =
  [ ("int", [%expr Pbt.Gens.int]); ("uint", [%expr Pbt.Gens.uint]) ]
  |> List.assoc_opt x

(* Either replace gen_ids by builtin generators or with identifiers to
   locally provided generators

   example:

   [@@pbt {| commutative[int, my_gen_int] |}]
   =>
   QCheck.pair (Pbt.Properties.int, my_gen_int)

   int belongs to builtin generators and is rewrited as the reference to the
   generator in Pbt.Properties module, whereas my_gen_int needs to be available
   at current scope *)
let replace_gens loc gen_ids =
  let replace gen_id =
    match builtin_generators loc gen_id with
    | Some gen -> gen
    | None -> Helpers.build_ident loc gen_id
  in
  List.map replace gen_ids

(* Split a list in two list of size (length list) / 2

   example:
   split_even_list [1; 2; 3; 4] => [1; 2] [3; 4]

   The reason we split only even list is because separated list are meant
   to be nested into pairs.
   A odd list would be translated to something like:
   [x; y; z] => (pair x (pair y z))
   In our case, the split is only applied on [y;z] *)
let split_even_list list =
  let n = List.length list in
  assert (n mod 2 = 0) ;
  let middle = n / 2 in
  let i = ref 0 in
  List.partition
    (fun _ ->
      i := !i + 1 ;
      !i <= middle)
    list

type 'a nested_pairs =
  | Pair of 'a nested_pairs * 'a nested_pairs
  | Double of 'a * 'a
  | Simple of 'a

(* nest_generators takes a list of generator and nest it into pairs

   example:
   nest_generators [a] => a
   nest_generators [a;b] => Double a b
   nest_generators [a;b;c] => Pair (Simple a) (Double b c)
   nest_generators [a;b;c;d] => Pair (Double a b) (Double c d)

   TODO:
     - implement and test empty list *)
let rec nest_generators gens =
  match gens with
  | [] -> failwith "TODO" (* insert unit ? *)
  | [ x ] -> Simple x
  | [ x; y ] -> Double (x, y)
  | gens when List.length gens mod 2 = 0 ->
      let (l1, l2) = split_even_list gens in
      Pair (nest_generators l1, nest_generators l2)
  | x :: xs -> Pair (Simple x, nest_generators xs)

(* nested_pairs_to_expr converts nested_pairs to Ast.expression,
   in order to be used in QCheck.Test.make generator argument

   QCheck.Test.make ~name:..
   <nested_pairs_to_expr loc nested_pairs>
   (fun .. -> ..) *)
let rec nested_pairs_to_expr loc = function
  | Simple expr -> expr
  | Pair (x, y) ->
      [%expr
        QCheck.pair
          [%e nested_pairs_to_expr loc x]
          [%e nested_pairs_to_expr loc y]]
  | Double (x, y) -> [%expr QCheck.pair [%e x] [%e y]]

(* nested_pairs_to_list converts nested_pairs to list of Ast.expression,
   in order to be used in the tested function

   QCheck.Test.make ~name:..
   <generators>
   (fun <pat> -> <property> <fun_name> <nested_pairs_to list nested_pairs>) *)
let rec nested_pairs_to_list = function
  | Simple x -> [ x ]
  | Double (x, y) -> [ x; y ]
  | Pair (x, y) ->
      let left = nested_pairs_to_list x in
      let right = nested_pairs_to_list y in
      left @ right
