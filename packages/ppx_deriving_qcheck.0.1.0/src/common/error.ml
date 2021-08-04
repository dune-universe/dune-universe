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

let default_loc = ref Location.none

let set_loc x = default_loc := x

let raise_errorf ?(loc = !default_loc) = Ppxlib__Location.raise_errorf ~loc

let internal_error ?loc () = raise_errorf ?loc "%s" "Internal error"

let syntax_error ?loc ~err () = raise_errorf ?loc "Syntax error : %c" err

let case_unsupported ?loc ~case () =
  raise_errorf ?loc "This case is not supported yet : %s" case

let property_unsupported ?loc ~property () =
  raise_errorf ?loc "The property %s is not supported in ppx_pbt" property

let property_gen_missing ?loc ~property ~required ~actual () =
  raise_errorf
    ?loc
    "The property %s requires %d gens, %d found"
    property
    required
    actual

let property_arg_missing ?loc ~property ~required ~actual () =
  raise_errorf
    ?loc
    "The property %s requires %d args, %d found"
    property
    required
    actual

let location_error ?loc ~msg () = raise_errorf ?loc "%s" msg
