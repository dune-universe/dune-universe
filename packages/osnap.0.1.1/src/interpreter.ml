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

open Spec
module Gen = Spec.Gen

(** [args] is an intern representation of function arguments *)
type ('fn, 'r) args =
  | Nil : ('r, 'r) args
  | Cons : 'a * ('fn, 'r) args -> ('a -> 'fn, 'r) args

(** [spec_to_args] instantiate arguments values using generators *)
let rec spec_to_args : type a b. (a, b) Spec.t -> (a, b) args = function
  | Result _ -> Nil
  | Arrow ({ gen; _ }, spec) ->
      let x = QCheck.Gen.generate1 gen in
      Cons (x, spec_to_args spec)

(** [expr] is an intern representation of a function application *)
type _ expr =
  | Apply : ('a -> 'b) expr * 'a expr -> 'b expr
  | Term : 'a -> 'a expr
  | Fun : ('a -> 'b) -> ('a -> 'b) expr

let rec args_to_expr : type a b. a expr -> (a, b) args -> b expr =
 fun expr args ->
  match args with
  | Nil -> expr
  | Cons (x, args) ->
      let expr = Apply (expr, Term x) in
      args_to_expr expr args

(** [spec_to_expr spec f] transforms a {!Spec.t} to {!expr} *)
let spec_to_expr spec f = spec_to_args spec |> args_to_expr (Fun f)

(** [interpret expr] interprets [expr] by evaluating function applications *)
let rec interpret : type a. a expr -> a = function
  | Apply (f, x) -> (interpret f) (interpret x)
  | Term x -> x
  | Fun f -> f

(**/**)

module Internal_for_tests = struct
  (** [spec_to_args] instantiate arguments values using generators *)
  let rec spec_to_args :
      type a b. Random.State.t -> (a, b) Spec.t -> (a, b) args =
   fun rand spec ->
    match spec with
    | Result _ -> Nil
    | Arrow ({ gen; _ }, spec) ->
        let x = QCheck.Gen.generate1 ~rand gen in
        Cons (x, spec_to_args rand spec)
end
