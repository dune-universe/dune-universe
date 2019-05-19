open Batteries;;
open Jhupllib;;
open OUnit2;;

type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Negate of expr
  | Integer of int
[@@deriving eq, ord, show, to_yojson]
;;

module Expr = struct
  type t = expr
  let compare = compare_expr
  let pp = pp_expr
  let to_yojson = expr_to_yojson;;
end;;

module Expr_registry =
struct
  module R = Witness_protection.Make_escorted(Expr);;
  include R;;
  include Witness_protection.Make_pp(R)(Expr);;
  include Witness_protection.Make_to_yojson(R)(Expr);;
end;;

type functor_test_type =
  | Functor_test_constructor of Expr_registry.escorted_witness
[@@deriving eq, ord, show, to_yojson]
;;

let make_tests () =
  Random.init 4; (* arbitrary seed *)
  let rand_tree () =
    let num_children = Random.int 10000 in
    let rec loop num_children =
      if num_children = 0 then
        Integer (Random.int 1000)
      else
        let pick = Random.int 3 in
        if pick = 0 then
          Negate(loop (num_children - 1))
        else
          let split = Random.int (num_children + 1) in
          let left = loop split in
          let right = loop (num_children - split) in
          if pick = 1 then Plus(left,right) else Minus(left,right)
    in
    loop num_children
  in

  let tree1 = rand_tree () in
  let tree2 = rand_tree () in
  let tree3 = rand_tree () in

  [
    "same_tree_same" >:: begin
      fun _ ->
        let registry = Expr_registry.empty_registry () in
        let w1 = Expr_registry.witness_of registry tree1 in
        let w1' = Expr_registry.witness_of registry tree1 in
        assert_equal w1 w1'
    end;
    "different_tree_different" >:: begin
      fun _ ->
        let registry = Expr_registry.empty_registry () in
        let w1 = Expr_registry.witness_of registry tree1 in
        let w2 = Expr_registry.witness_of registry tree2 in
        assert_bool
          "Witnesses were expected to be distinct!"
          (not @@ Expr_registry.equal_witness w1 w2)
    end;
    "witness_not_found_empty" >:: begin
      fun _ ->
        let registry1 = Expr_registry.empty_registry () in
        let registry2 = Expr_registry.empty_registry () in
        let w = Expr_registry.witness_of registry2 tree1 in
        assert_raises Not_found
          (fun () -> Expr_registry.element_of registry1 w)
    end;
    "witness_not_found_non_empty" >:: begin
      fun _ ->
        let registry1 = Expr_registry.empty_registry () in
        let _ = Expr_registry.witness_of registry1 tree1 in
        let _ = Expr_registry.witness_of registry1 tree2 in
        let registry2 = Expr_registry.empty_registry () in
        let w = Expr_registry.witness_of registry2 tree1 in
        assert_raises Not_found
          (fun () -> Expr_registry.element_of registry1 w)
    end;
    "get_witness_value" >:: begin
      fun _ ->
        let registry = Expr_registry.empty_registry () in
        let _ = Expr_registry.witness_of registry tree1 in
        let _ = Expr_registry.witness_of registry tree2 in
        let w3 = Expr_registry.witness_of registry tree3 in
        assert_equal tree3 @@ Expr_registry.element_of registry w3
    end;
  ]
;;

let tests = "Witness_protection tests" >::: make_tests ();;
