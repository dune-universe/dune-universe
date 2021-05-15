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

(* --- Building patterns --- *)
let build_pattern loc pat =
  { ppat_desc = pat; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = [] }

let build_pattern_var loc x = build_pattern loc @@ Ppat_var { txt = x; loc }

let build_pattern_default loc = build_pattern loc @@ Ppat_any

(* --- Building expressions --- *)
let build_expression loc exp =
  { pexp_desc = exp; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = [] }

let build_value_binding loc pat expr =
  { pvb_pat = pat; pvb_expr = expr; pvb_attributes = []; pvb_loc = loc }

let build_let loc values_binding exp =
  build_expression loc @@ Pexp_let (Nonrecursive, values_binding, exp)

let build_string loc str =
  build_expression loc (Pexp_constant (Pconst_string (str, loc, None)))

let rec build_list loc = function
  | [] -> [%expr []]
  | x :: xs -> [%expr [%e x] :: [%e build_list loc xs]]

let build_apply loc to_apply args =
  build_expression loc @@ Pexp_apply (to_apply, args)

let build_ident loc x =
  build_expression loc @@ Pexp_ident { txt = Lident x; loc }
