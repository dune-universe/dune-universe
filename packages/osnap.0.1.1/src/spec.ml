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

module Gen = QCheck.Gen

type 'a gen = 'a Gen.t

type 'a printer = 'a -> string

type 'a spec = { gen : 'a gen; printer : 'a printer }

type ('fn, 'r) t =
  | Result : 'a printer -> ('a, 'a) t
  | Arrow : 'a spec * ('fn, 'r) t -> ('a -> 'fn, 'r) t

let unit = { gen = Gen.unit; printer = Unit.to_string }

let bool = { gen = Gen.bool; printer = Bool.to_string }

let int = { gen = Gen.int; printer = Int.to_string }

let float = { gen = Gen.float; printer = Float.to_string }

let char = { gen = Gen.char; printer = Format.sprintf "%c" }

let string = { gen = Gen.string; printer = (fun x -> x) }

let option spec =
  {
    gen = Gen.opt spec.gen;
    printer = Option.fold ~none:"None" ~some:spec.printer;
  }

let printer_list f l =
  let rec printer_elements = function
    | [] -> ""
    | [ x ] -> f x
    | x :: xs -> Printf.sprintf "%s , %s" (f x) (printer_elements xs)
  in
  Printf.sprintf "[%s]" (printer_elements l)

let array spec =
  {
    gen = Gen.array spec.gen;
    printer = (fun x -> Array.to_list x |> printer_list spec.printer);
  }

let list spec =
  let printer = printer_list spec.printer in
  let gen = Gen.list spec.gen in
  { gen; printer }

let ( ^> ) x y = Arrow (x, y)

let ( ^>> ) x y = Arrow (x, Result y)
