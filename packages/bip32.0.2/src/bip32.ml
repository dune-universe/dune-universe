(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Secp256k1

module type CRYPTO = sig
  val sha256 : Cstruct.t -> Cstruct.t
  val ripemd160 : Cstruct.t -> Cstruct.t
  val hmac_sha512 : key:Cstruct.t -> Cstruct.t -> Cstruct.t

  val ctx : Context.t
end

let hardened i =
  Int32.logand i 0x8000_0000l <> 0l

let pp_print_path ppf i =
  if hardened i then
    Format.fprintf ppf "%ld'" Int32.(logand i 0x7fff_ffffl)
  else
    Format.fprintf ppf "%ld" i

type 'a t = {
  k : 'a Key.t ;
  c : Cstruct.t ;
  path : Int32.t list ;
  parent : Cstruct.t ;
}

let create_key ?(parent=Cstruct.create 20) k c path = { k ; c ; path ; parent }

module type S = sig
  val pp : Format.formatter -> _ t -> unit
  val of_entropy : Cstruct.t -> (Key.secret t, string) result
  val of_entropy_exn : Cstruct.t -> Key.secret t
  val neuterize : _ t -> Key.public t
  val derive : 'a t -> Int32.t -> 'a t
  val derive_path : 'a t -> Int32.t list -> 'a t

  val secret_of_bytes : Cstruct.t -> Key.secret t option
  val secret_of_bytes_exn : Cstruct.t -> Key.secret t
  val public_of_bytes : Cstruct.t -> Key.public t option
  val public_of_bytes_exn : Cstruct.t -> Key.public t
  val to_cstruct : 'a t -> Cstruct.t
end

module Make (Crypto : CRYPTO) = struct
  open Crypto
  let fingerprint k =
    Key.to_bytes ~compress:true ctx k |>
    Cstruct.of_bigarray |>
    Crypto.sha256 |>
    Crypto.ripemd160

  let pp_k ppf k =
    Hex.pp ppf (Key.to_bytes ctx k |> Cstruct.of_bigarray |> Hex.of_cstruct)

  let pp :
    type a. Format.formatter -> a t -> unit = fun ppf { k ; c ; path ; parent } ->
    Format.fprintf ppf "@[<hov 0>key %a@ chaincode %a@ path %a@ parent %a@]"
      pp_k k
      Hex.pp (Hex.of_cstruct c)
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '/')
         pp_print_path) (List.rev path)
      Hex.pp (Hex.of_cstruct parent)

  let of_entropy entropy =
    let key = Cstruct.of_string "Bitcoin seed" in
    let m = Crypto.hmac_sha512 ~key entropy in
    match Key.read_sk ctx m.buffer with
    | Error msg -> Error msg
    | Ok k -> Ok (create_key k (Cstruct.sub m 32 32) [])

  let of_entropy_exn entropy =
    match of_entropy entropy with
    | Error msg -> invalid_arg msg
    | Ok sk -> sk

  let neuterize : type a. a t -> Key.public t = fun k ->
    { k with k = Key.neuterize_exn ctx k.k }

  let derive : type a. a t -> Int32.t -> a t = fun { k ; c = key ; path } i ->
      match k, hardened i with
      | Key.Pk _, true ->
        invalid_arg "derive: cannot derive hardened index" ;
      | Key.Sk _, _ ->
        let buf = Cstruct.create 37 in
        let (_:int) = if hardened i then
          Key.write ctx buf.buffer ~pos:1 k
        else begin
          let pk = Key.neuterize_exn ctx k in
          Key.write ~compress:true ctx buf.buffer pk
        end in () ;
        Cstruct.BE.set_uint32 buf 33 i ;
        let derived = Crypto.hmac_sha512 ~key buf in
        let k' = Key.add_tweak ctx k derived.buffer in
        let c' = Cstruct.sub derived 32 32 in
        create_key ~parent:(fingerprint (Key.neuterize_exn ctx k)) k' c' (i :: path)
      | Key.Pk _, _ ->
        let cs = Cstruct.create 37 in
        let (_:int) = Key.write ~compress:true ctx cs.buffer k in
        let derived = Crypto.hmac_sha512 ~key cs in
        let k' = Key.add_tweak ctx k derived.buffer in
        let c' = Cstruct.sub derived 32 32 in
        create_key ~parent:(fingerprint k) k' c' (i :: path)

  let derive_path :
    type a. a t -> Int32.t list -> a t = fun k path ->
    ListLabels.fold_left path ~init:k ~f:derive

  let secret_of_bytes_exn cs =
    let _depth = Cstruct.get_uint8 cs 0 in
    let parent = Cstruct.sub cs 1 4 in
    let child_number = Cstruct.BE.get_uint32 cs 5 in
    let chaincode = Cstruct.sub cs 9 32 in
    let secret = Key.read_sk_exn ctx cs.buffer ~pos:41 in
    create_key ~parent secret chaincode [child_number]

  let public_of_bytes_exn cs =
    let _depth = Cstruct.get_uint8 cs 0 in
    let parent = Cstruct.sub cs 1 4 in
    let child_number = Cstruct.BE.get_uint32 cs 5 in
    let chaincode = Cstruct.sub cs 9 32 in
    let public = Key.read_pk_exn ctx cs.buffer ~pos:40 in
    create_key ~parent public chaincode [child_number]

  let secret_of_bytes cs =
    try Some (secret_of_bytes_exn cs) with _ -> None
  let public_of_bytes cs =
    try Some (public_of_bytes_exn cs) with _ -> None

  let secret_of_bytes_exn cs =
    match secret_of_bytes cs with
    | None -> invalid_arg "secret_of_bytes_exn"
    | Some sk -> sk

  let public_of_bytes_exn cs =
    match public_of_bytes cs with
    | None -> invalid_arg "public_of_bytes_exn"
    | Some pk -> pk

  let to_cstruct : type a. a t -> Cstruct.t = fun { k ; c ; path ; parent } ->
    let buf = Cstruct.create 74 in
    Cstruct.set_uint8 buf 0 (List.length path) ;
    Cstruct.blit parent 0 buf 1 4 ;
    Cstruct.BE.set_uint32 buf 5 (match path with [] -> 0l  | i :: _ -> i) ;
    Cstruct.blit c 0 buf 9 32 ;
    let _nb_written = begin match k with
      | Key.Sk _ -> Key.write ctx buf.buffer ~pos:42 k
      | Key.Pk _ -> Key.write ~compress:true ctx buf.buffer ~pos:41 k
    end in
    buf
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
