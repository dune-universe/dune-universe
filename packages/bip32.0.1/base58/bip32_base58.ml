(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Secp256k1
open Bip32

module type S = sig
  val of_base58_sk : Base58.Bitcoin.t -> secret key option
  val of_base58_pk : Base58.Bitcoin.t -> public key option
  val of_base58_sk_exn : Base58.Bitcoin.t -> secret key
  val of_base58_pk_exn : Base58.Bitcoin.t -> public key
  val to_base58 : ?testnet:bool -> _ key -> Base58.Bitcoin.t
end

module Make (Crypto : CRYPTO) = struct
  module BIP32 = Make(Crypto)
  open Crypto
  open BIP32

  let to_base58 :
    type a. ?testnet:bool -> a key -> Base58.Bitcoin.t = fun ?(testnet=false) s ->
    let version =
      match s.k with
      | Sk _ -> Base58.Bitcoin.(if testnet then Testnet_BIP32_priv else BIP32_priv)
      | Pk _ -> Base58.Bitcoin.(if testnet then Testnet_BIP32_pub else BIP32_pub)
    in
    Base58.Bitcoin.create ~version ~payload:(to_bytes s)

  let of_base58_sk { Base58.Bitcoin.version ; payload } =
    match version with
    | BIP32_priv | Testnet_BIP32_priv ->
      secret_of_bytes (Cstruct.of_string payload)
    | _ -> None

  let of_base58_pk { Base58.Bitcoin.version ; payload } =
    match version with
    | BIP32_pub | Testnet_BIP32_pub ->
      public_of_bytes (Cstruct.of_string payload)
    | _ -> None

  let of_base58_sk_exn b58 =
    match of_base58_sk b58 with
    | Some sk -> sk
    | None -> invalid_arg "of_base58_sk_exn"

  let of_base58_pk_exn b58 =
    match of_base58_pk b58 with
    | Some pk -> pk
    | None -> invalid_arg "of_base58_pk_exn"
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
