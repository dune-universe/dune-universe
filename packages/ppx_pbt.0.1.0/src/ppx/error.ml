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

exception InternalError

exception SyntaxError of string

exception CaseUnsupported of string

exception PropertyNotSupported of string

exception PropertyGeneratorsMissing of string * int * int

let syntax_error c = raise (SyntaxError (Format.sprintf "%c" c))

let print_exception = function
  | SyntaxError s -> Format.printf "SyntaxError (%s)\n" s
  | CaseUnsupported s -> Format.printf "CaseUnsupported in (%s)\n" s
  | PropertyNotSupported s ->
      Format.printf "The property %s is not supported in ppx_pbt\n" s
  | PropertyGeneratorsMissing (s, actual, expected) ->
      Format.printf
        "Property %s requires %d generators, here %d are applied\n"
        s
        actual
        expected
  | e -> Format.printf "InternalError (%s)\n" (Printexc.to_string e)
