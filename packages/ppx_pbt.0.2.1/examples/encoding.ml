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

(** Data-encoding can be encoded and decoded. We want to test if the encoding
    is roundtrip

    roundtrip_data_encoding encoding:
      forall x:
        x = decode (encode x)
*)

open Data_encoding

let arb_int8 = QCheck.(0 -- 127)

let arb_int16 = QCheck.(0 -- 32767)

let encode_int8 = int8 [@@pbt "roundtrip_data_encoding[arb_int8]"]

let encode_int16 = int16 [@@pbt "roundtrip_data_encoding[arb_int16]"]

let encode_int32 = int32 [@@pbt "roundtrip_data_encoding[int32]"]

let encode_int64 = int64 [@@pbt "roundtrip_data_encoding[int64]"]

let encode_float = float [@@pbt "roundtrip_data_encoding[float]"]

let encode_bool = bool [@@pbt "roundtrip_data_encoding[bool]"]

let encode_string = string [@@pbt "roundtrip_data_encoding[string]"]
