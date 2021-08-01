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

include Encoding_intf

module Hash = struct
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
    | None -> Error (`Msg (Format.asprintf "Failed to read b58check_encoding data"))

  let short_hash_string = Repr.(unstage (short_hash string))

  let short_hash_staged = Repr.stage @@ fun ?seed t -> short_hash_string ?seed (H.to_raw_string t)

  let t : t Repr.t =
    Repr.map ~pp ~of_string
      Repr.(string_of (`Fixed H.digest_size))
      ~short_hash:short_hash_staged H.of_raw_string H.to_raw_string

  let short_hash =
    let f = short_hash_string ?seed:None in
    fun t -> f (H.to_raw_string t)

  let hash_size = H.digest_size

  let hash = H.digesti_string
end

module Key : KEY with type t = Hash.t = struct
  type t = Hash.t [@@deriving repr]

  let hash = Repr.(unstage (short_hash Hash.t)) ?seed:None

  let hash_size = 30

  let equal = Repr.(unstage (equal Hash.t))

  let encode = Repr.(unstage (to_bin_string Hash.t))

  let encoded_size = Hash.hash_size

  let decode_bin = Repr.(unstage (decode_bin Hash.t))

  let decode s off =
    let _, v = decode_bin s off in
    v
end

module Int63 = struct
  include Optint.Int63

  let t : t Repr.t =
    let open Repr in
    (map int64) of_int64 to_int64
    |> like ~pp:Optint.Int63.pp ~equal:(stage Optint.Int63.equal)
         ~compare:(stage Optint.Int63.compare)
end

type int63 = Int63.t [@@deriving repr]

module Val = struct
  type t = int63 * int * char [@@deriving repr]

  let to_bin_string = Repr.(unstage (to_bin_string (triple int63_t int32 char)))

  let encode (off, len, kind) = to_bin_string (off, Int32.of_int len, kind)

  let decode_bin = Repr.(unstage (decode_bin (triple int63_t int32 char)))

  let decode s off =
    let off, len, kind = snd (decode_bin s off) in
    (off, Int32.to_int len, kind)

  let encoded_size = (64 / 8) + (32 / 8) + 1
end

let random_char () = char_of_int (33 + Random.int 94)

module H = Irmin.Hash.Typed (Hash) (Val)

let random () : Key.t * Val.t =
  let value = (Random.int 1000 |> Int63.of_int, Random.int 1000, random_char ()) in
  (H.hash value, value)
