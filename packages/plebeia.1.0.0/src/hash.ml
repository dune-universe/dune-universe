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
(** The prefix of hash, fixed width 224 bits *)

open Utils.Open

module Blake2B_28 = struct
  let of_string s =
    let open Blake2.Blake2b in
    let b = init 28 in
    update b (Bytes.of_string s);
    let Hash bs = final b in
    Bytes.to_string bs

  let of_strings ss =
    let open Blake2.Blake2b in
    let b = init 28 in
    List.iter (fun s ->
        update b (Bytes.of_string s)) ss;
    let Hash bs = final b in
    Bytes.to_string bs
end

let test () =
  assert (Hex.of_string @@ Blake2B_28.of_string "Replacing SHA1 with the more secure function"
          = `Hex "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578");
  assert (Hex.of_string @@ Blake2B_28.of_strings ["Replacing SHA1 with the "; "more secure function" ]
          = `Hex "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578")
(* obtained by
#!/usr/local/bin/python3

from hashlib import blake2b

h = blake2b(digest_size=28)
h.update(b'Replacing SHA1 with the more secure function')
print (h.hexdigest())
*)

let hash = Blake2B_28.of_string
let hash_list = Blake2B_28.of_strings

type t = string

let to_string x = x

let of_string x = assert (String.length x = 28); x
(* This module is only for internal use.
   Error recovery is not required. *)

let to_hex x = Hex.of_string x
let to_hex_string x = Hex.show @@ Hex.of_string x
let of_hex h = of_string @@ Hex.to_string h

let set_last_2bits n s =
  let len = String.length s in
  if len <> 28 then failwithf "reset_last_2bits: len=%d <> 28" len;
  let bs = Bytes.of_string s in
  Bytes.unsafe_set bs 27
  @@ Char.chr @@ Char.code (Bytes.unsafe_get bs 27) land 0xfc lor n;
  Bytes.to_string bs

let zero = String.make 28 '\000'

let encoding = Data_encoding.string
