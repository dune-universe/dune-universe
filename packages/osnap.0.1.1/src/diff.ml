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

type t =
  | Same  (** old and new version are equals *)
  | Diff of string  (** difference between old and new *)
  | New of string  (** only new version *)

let pp fmt = function
  | Same -> Format.pp_print_string fmt "Same"
  | Diff s -> Format.pp_print_string fmt @@ "Diff " ^ s
  | New s -> Format.pp_print_string fmt @@ "New " ^ s

(** [diff prev next] computes diff between [prev] and [next] using Patdiff
    If prev is absent, returns [New [next]] *)
let diff prev next =
  let config = Patdiff.Configuration.default in
  match prev with
  | None -> New next
  | Some prev -> (
      let open Patdiff_kernel.Diff_input in
      let prev = { name = "prev"; text = prev } in
      let next = { name = "next"; text = next } in

      match Patdiff__Compare_core.diff_strings ~prev ~next config with
      | `Same -> Same
      | `Different s -> Diff s)
