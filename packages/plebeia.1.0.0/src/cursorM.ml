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
(** Cursor (zipper) based tree operations, in monadic style *)

open Cursor

module Base = struct
  type 'a t = Cursor.t -> Cursor.t * ('a, Error.t) Result.t

  let return : 'a -> 'a t = fun a c -> (c, Ok a)

  let bind : 'a t -> ('a -> 'b t) -> 'b t = fun at f c -> 
    match at c with
    | (c, Error e) -> (c, Error e)
    | (c, Ok a) -> f a c 
end

include Monad.Make1(Base)

let segments c = c, Ok (segs_of_cursor c)
let local_segment c = c, Ok (local_seg_of_cursor c)
let view c = let c, v = view c in c, Ok v
let index c = c, Ok (index c)

let go_below_bud c = 
  match go_below_bud c with
  | Error e -> c, Error e
  | Ok None -> c, Error (Access Empty_bud)
  | Ok (Some c) -> c, Ok ()

let wrap_unit f c = 
  match f c with
  | Ok c -> c, Ok ()
  | Error e -> c, Error e

let go_side side = wrap_unit @@ go_side side
let go_down_extender = wrap_unit go_down_extender
let go_up = wrap_unit go_up
let go_top = wrap_unit go_top
let go_up_to_bud = wrap_unit go_up_to_bud
let subtree seg = wrap_unit (fun c -> subtree c seg)

(* XXX should have an original version *)
let get seg c = 
  match get c seg with
  | Ok (c, v) -> c, Ok v
  | Error e -> c, Error e

let get_value seg c = 
  match get_value c seg with
  | Ok (c, v) -> c, Ok v
  | Error e -> c, Error e

let delete seg = wrap_unit (fun c -> delete c seg)
let alter seg f = wrap_unit (fun c -> alter c seg f)
let update seg v = wrap_unit (fun c -> update c seg v)
let upsert seg v = wrap_unit (fun c -> upsert c seg v)
let insert seg v = wrap_unit (fun c -> insert c seg v)
let create_subtree seg = wrap_unit (fun c -> create_subtree c seg)
let subtree_or_create seg = wrap_unit (fun c -> subtree_or_create c seg)

let stat c = 
  let Cursor (_,_,{ stat ; _ }) = c in
  c, Ok stat

let may_forget c = 
  match may_forget c with
  | None -> c, Ok ()
  | Some c -> c, Ok ()
