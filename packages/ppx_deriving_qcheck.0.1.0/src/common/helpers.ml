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
module P = Ast_helpers.Pattern

let mk_loc ?loc x =
  match loc with
  | None -> { txt = x; loc = Location.none }
  | Some loc -> { txt = x; loc }

let opt f = try Some (f ()) with _ -> None

module Info = struct
  type t = {
    stri_name : string;
    stri_attr : attribute option;
    stri_loc : Location.t;
  }

  let create_info ?(name = "") ?attr ?(loc = Location.none) () =
    { stri_name = name; stri_attr = attr; stri_loc = loc }

  let update_name name info = { info with stri_name = name }

  let get_name x = x.stri_name

  let get_attribute x = x.stri_attr

  let get_loc x = x.stri_loc
end

module Pairs = struct
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

  let rec nest_generators gens =
    match gens with
    | [] ->
        let loc = Location.none in
        Simple [%expr Pbt.Gens.unit]
    | [ x ] -> Simple x
    | [ x; y ] -> Double (x, y)
    | gens when List.length gens mod 2 = 0 ->
        let (l1, l2) = split_even_list gens in
        Pair (nest_generators l1, nest_generators l2)
    | x :: xs -> Pair (Simple x, nest_generators xs)

  let rec nested_pairs_to_expr loc = function
    | Simple expr -> expr
    | Pair (x, y) ->
        [%expr
          QCheck.pair
            [%e nested_pairs_to_expr loc x]
            [%e nested_pairs_to_expr loc y]]
    | Double (x, y) -> [%expr QCheck.pair [%e x] [%e y]]

  let rec nested_pairs_to_list = function
    | Simple x -> [ x ]
    | Double (x, y) -> [ x; y ]
    | Pair (x, y) ->
        let left = nested_pairs_to_list x in
        let right = nested_pairs_to_list y in
        left @ right

  let names_from_gens f gens =
    let id = ref 0 in
    let create_fresh_name i =
      let x = !i in
      i := !i + 1 ;
      f @@ string_of_int x
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

  let pattern_from_gens loc f gens =
    let rec create_pattern loc = function
      | Pair (x, y) ->
          [%pat? ([%p create_pattern loc x], [%p create_pattern loc y])]
      | Double (x, y) ->
          let arg_x = P.ppat_var ~loc x in
          let arg_y = P.ppat_var ~loc y in
          Ppat_tuple [ arg_x; arg_y ] |> P.pattern ~loc
      | Simple x -> P.ppat_var ~loc x
    in
    let args = names_from_gens f gens in
    (create_pattern loc args, args)
end
