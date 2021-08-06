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

(** Modules handling generators *)

open Ppxlib

(** [infer_gens_from_sig loc sig] extract the generators needed for the signature
    item, which must be a Psig_value.

    {{:https://github.com/vch9/ppx_deriving_qcheck}ppx_deriving_qcheck} is used to
    create generators from core_type inside the signature.
    If the deriver was not able to derive a core_type inside [sig], it is replaced
    by the value None. *)
val infer_gens_from_sig :
  loc:location -> signature_item -> expression option list

(** [create_gens loc sig_item property gens] extract QCheck generators from the
    combination of [gens] and [sig_item].

    In priority, generators are taken from [gens]. In the case where [sig_item] is
    present, we can infer missing generators from [gens] based on the signature
    representation of the function.

    In the case where [property] is a built-in property of this ppx, we can perform
    2 checks:
    - Check that the number of generator is correct.
    - TODO: Check that the infered generators respect the property's type. *)
val create_gens :
  loc:location ->
  signature_item option ->
  Properties.property_name ->
  Properties.gen list ->
  expression * expression Common.Helpers.Pairs.nested_pairs
