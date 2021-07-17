(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

module H = Digestif.Make_BLAKE2B (struct
  let digest_size = 32
end)

type t = H.t

let prefix = "\079\199" (* Co(52) *)

let pp ppf t =
  let s = H.to_raw_string t in
  Base58.pp ppf (Base58.encode ~prefix s)

let of_string : string -> (t, [ `Msg of string ]) result =
 fun x ->
  match Base58.decode ~prefix (Base58 x) with
  | Some x -> Ok (H.of_raw_string x)
  | None ->
      Error (`Msg (Format.asprintf "Failed to read b58check_encoding data"))

let short_hash_string = Repr.(unstage (short_hash string))

let short_hash_staged =
  Repr.stage @@ fun ?seed t -> short_hash_string ?seed (H.to_raw_string t)

let t : t Repr.t =
  Repr.map ~pp ~of_string
    Repr.(string_of (`Fixed H.digest_size))
    ~short_hash:short_hash_staged H.of_raw_string H.to_raw_string

let short_hash =
  let f = short_hash_string ?seed:None in
  fun t -> f (H.to_raw_string t)

let hash_size = H.digest_size
let hash = H.digesti_string
