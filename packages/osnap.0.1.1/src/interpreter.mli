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

(** This module handles internal representation of function. We use
    module {!Spec} to generate type {!args}. This last is latter on used
    to evaluate function with these arguments using type {!expr}.

    {!args} is encoded inside snapshots, values of types {!args} are used
    to evaluate new function under test with the same arguments.
*)

type ('fn, 'r) args =
  | Nil : ('r, 'r) args
  | Cons : 'a * ('fn, 'r) args -> ('a -> 'fn, 'r) args

(** [spec_to_args spec] instantiate arguments values using generators inside [spec] *)
val spec_to_args : ('a, 'b) Spec.t -> ('a, 'b) args

(** [expr] is an intern representation of a function application,
    Term inside [expr] are create using {!args}. *)
type _ expr =
  | Apply : ('a -> 'b) expr * 'a expr -> 'b expr
  | Term : 'a -> 'a expr
  | Fun : ('a -> 'b) -> ('a -> 'b) expr

(** [args_to_expr f args] create a function application representation using
    type {!expr}. [f] must be a term [Fun f], the expression will then be
    evaluated on that function [f] *)
val args_to_expr : 'a expr -> ('a, 'b) args -> 'b expr

(** [spec_to_expr spec] creates an {!expr} using {!spec_to_args} and {!args_to_expr} *)
val spec_to_expr : ('a -> 'b, 'c) Spec.t -> ('a -> 'b) -> 'c expr

(** [interpret expr] evaluates [expr], providing the function a full aplication,
    returning its result. *)
val interpret : 'a expr -> 'a

(**/**)

module Internal_for_tests : sig
  val spec_to_args : Random.State.t -> ('a, 'b) Spec.t -> ('a, 'b) args
end
