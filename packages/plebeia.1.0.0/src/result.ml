(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
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
(* "Error" monad *)

include Monad.Make2(struct
    type ('a, 'b) t = ('a, 'b) result
    let return x = Ok x

    let bind x f = match x with
      | Error e -> Error e
      | Ok x -> f x
end)

(* optimized *)
let map f y = match y with
  | Ok x -> Ok (f x)
  | Error e -> Error e

let (>>|) y f = map f y

let from_Ok = function
  | Ok x -> x
  | Error _ -> failwith "Expected an Ok"

let from_Error = function
  | Ok _ -> failwith "Expected an Error"
  | Error e -> e

let default r f = match r with
  | Ok x -> x
  | Error e -> f e

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let map_error f r = match r with
  | Ok x -> Ok x
  | Error e -> Error (f e)
