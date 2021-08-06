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

module Properties = struct
  let commutative f x y = f x y = f y x

  let associative f x y z = f (f x y) z = f x (f y z)

  let neutral_left f elt x = f elt x = x

  let neutral_right f elt x = f x elt = x

  let neutrals f neutral x =
    neutral_right f neutral x && neutral_left f neutral x

  let capped_left f cap x = f cap x = cap

  let capped_right f cap x = f x cap = cap

  let capped f cap x = capped_left f cap x && capped_right f cap x

  let oracle f oracle x y = f x y = oracle x y

  let absorb_left f absorb x = f absorb x = absorb

  let absorb_right f absorb x = f x absorb = absorb

  let absorbs f absorb x = absorb_left f absorb x && absorb_right f absorb x

  let floored_left f floor x = f floor x = floor

  let floored_right f floor x = f x floor = floor

  let floored f floor x = floored_left f floor x && floored_right f floor x

  let roundtrip f g x = f (g x) = x

  let roundtrip_data_encoding encoding x =
    let encoded_x = Data_encoding.Json.construct encoding x in
    let decoded_x = Data_encoding.Json.destruct encoding encoded_x in
    x = decoded_x

  let from_string ?(loc = Location.none) = function
    | "commutative" -> [%expr Pbt.Properties.commutative]
    | "associative" -> [%expr Pbt.Properties.associative]
    | "neutral_left" -> [%expr Pbt.Properties.neutral_left]
    | "neutral_right" -> [%expr Pbt.Properties.neutral_right]
    | "neutrals" -> [%expr Pbt.Properties.neutrals]
    | "capped_left" -> [%expr Pbt.Properties.capped_left]
    | "capped_right" -> [%expr Pbt.Properties.capped_right]
    | "capped" -> [%expr Pbt.Properties.capped]
    | "oracle" -> [%expr Pbt.Properties.oracle]
    | "absorb_left" -> [%expr Pbt.Properties.absorb_left]
    | "absorb_right" -> [%expr Pbt.Properties.absorb_right]
    | "absorbs" -> [%expr Pbt.Properties.absorbs]
    | "floored_left" -> [%expr Pbt.Properties.floored_left]
    | "floored_right" -> [%expr Pbt.Properties.floored_right]
    | "floored" -> [%expr Pbt.Properties.floored]
    | "roundtrip" -> [%expr Pbt.Properties.roundtrip]
    | "roundtrip_data_encoding" ->
        [%expr Pbt.Properties.roundtrip_data_encoding]
    | s ->
        {
          pexp_desc = Pexp_ident { txt = Lident s; loc };
          pexp_loc = loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
        }

  let nb_of_gens_args = function
    | "commutative" -> Some (2, 0)
    | "associative" -> Some (3, 0)
    | "neutral_left" -> Some (1, 1)
    | "neutral_right" -> Some (1, 1)
    | "neutrals" -> Some (1, 1)
    | "capped" -> Some (1, 1)
    | "capped_left" -> Some (1, 1)
    | "capped_right" -> Some (1, 1)
    | "floored_left" -> Some (1, 1)
    | "floored_right" -> Some (1, 1)
    | "floored" -> Some (1, 1)
    | "oracle" -> Some (2, 1)
    | "absorb_left" -> Some (1, 1)
    | "absorb_right" -> Some (1, 1)
    | "absorbs" -> Some (1, 1)
    | "roundtrip" -> Some (1, 1)
    | "roundtrip_data_encoding" -> Some (1, 0)
    | _ -> None

  let nb_of_gens s =
    Option.fold ~some:(fun x -> Some (fst x)) ~none:None @@ nb_of_gens_args s

  let nb_of_args s =
    Option.fold ~some:(fun x -> Some (snd x)) ~none:None @@ nb_of_gens_args s
end
