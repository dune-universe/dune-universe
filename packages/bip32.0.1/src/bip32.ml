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

type secret = Secret.t
type public = Public.t

type _ kind =
  | Sk : secret -> secret kind
  | Pk : public -> public kind

type 'a key = {
  k : 'a kind ;
  c : Cstruct.t ;
  path : Int32.t list ;
  parent : Cstruct.t ;
}

let create_key ?(parent=Cstruct.create 20) k c path = { k ; c ; path ; parent }

module type S = sig
  val pp : Format.formatter -> _ key -> unit
  val of_entropy : Cstruct.t -> secret key option
  val of_entropy_exn : Cstruct.t -> secret key
  val neuterize : _ key -> public key
  val derive : 'a key -> Int32.t -> 'a key
  val derive_path : 'a key -> Int32.t list -> 'a key

  val secret_of_bytes : Cstruct.t -> secret key option
  val secret_of_bytes_exn : Cstruct.t -> secret key
  val public_of_bytes : Cstruct.t -> public key option
  val public_of_bytes_exn : Cstruct.t -> public key
  val to_bytes : 'a key -> string
  val to_cstruct : 'a key -> Cstruct.t
end

module Make (Crypto : CRYPTO) = struct
  open Crypto
  let fingerprint k =
    Public.to_bytes ~compress:true ctx k |>
    Cstruct.of_bigarray |>
    Crypto.sha256 |>
    Crypto.ripemd160

  let pp_kind :
    type a. Format.formatter -> a kind -> unit = fun ppf -> function
    | Sk sk ->
      Hex.pp ppf
        (Secret.to_bytes sk |> Cstruct.of_bigarray |> Hex.of_cstruct)
    | Pk pk ->
      Hex.pp ppf
        (Public.to_bytes ~compress:true ctx pk |>
         Cstruct.of_bigarray |>
         Hex.of_cstruct)

  let pp :
    type a. Format.formatter -> a key -> unit = fun ppf { k ; c ; path ; parent } ->
    Format.fprintf ppf "@[<hov 0>key %a@ chaincode %a@ path %a@ parent %a@]"
      pp_kind k
      Hex.pp (Hex.of_cstruct c)
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '/')
         pp_print_path) (List.rev path)
      Hex.pp (Hex.of_cstruct parent)

  let of_entropy entropy =
    let key = Cstruct.of_string "Bitcoin seed" in
    let m = Crypto.hmac_sha512 ~key entropy in
    match Secret.read ctx m.buffer with
    | None -> None
    | Some k -> Some (create_key (Sk k) (Cstruct.sub m 32 32) [])

  let of_entropy_exn entropy =
    match of_entropy entropy with
    | None -> invalid_arg "of_entropy_exn"
    | Some sk -> sk

  let neuterize : type a. a key -> public key = fun k ->
    match k.k with
    | Sk sk ->
      let pk = Public.of_secret ctx sk in
      { k with k = Pk pk }
    | Pk _ -> k

  let derive : type a. a key -> Int32.t -> a key = fun { k ; c = key ; path } i ->
      match k, hardened i with
      | Pk _, true ->
        invalid_arg "derive: cannot derive hardened index" ;
      | Sk k, _ ->
        let buf = Cstruct.create 37 in
        if hardened i then
          Secret.write buf.buffer ~pos:1 k
        else begin
          let pk = Public.of_secret ctx k in
          let (_:int) = Public.write ~compress:true ctx buf.buffer pk in
          ()
        end ;
        Cstruct.BE.set_uint32 buf 33 i ;
        let derived = Crypto.hmac_sha512 ~key buf in
        let k' = Secret.add_tweak ctx k derived.buffer in
        let c' = Cstruct.sub derived 32 32 in
        create_key ~parent:(fingerprint (Public.of_secret ctx k)) (Sk k') c' (i :: path)
      | Pk k, _ ->
        let cs = Cstruct.create 37 in
        let (_:int) = Public.write ~compress:true ctx cs.buffer k in
        let derived = Crypto.hmac_sha512 ~key cs in
        let k' = Public.add_tweak ctx k derived.buffer in
        let c' = Cstruct.sub derived 32 32 in
        create_key ~parent:(fingerprint k) (Pk k') c' (i :: path)

  let derive_path :
    type a. a key -> Int32.t list -> a key = fun k path ->
    ListLabels.fold_left path ~init:k ~f:derive

  let secret_of_bytes_exn cs =
    let _depth = Cstruct.get_uint8 cs 0 in
    let parent = Cstruct.sub cs 1 4 in
    let child_number = Cstruct.BE.get_uint32 cs 5 in
    let chaincode = Cstruct.sub cs 9 32 in
    let secret = Secret.read_exn ctx cs.buffer ~pos:41 in
    create_key ~parent (Sk secret) chaincode [child_number]

  let public_of_bytes_exn cs =
    let _depth = Cstruct.get_uint8 cs 0 in
    let parent = Cstruct.sub cs 1 4 in
    let child_number = Cstruct.BE.get_uint32 cs 5 in
    let chaincode = Cstruct.sub cs 9 32 in
    let public = Public.read_exn ctx cs.buffer ~pos:40 in
    create_key ~parent (Pk public) chaincode [child_number]

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

  let to_bytes : type a. a key -> string = fun { k ; c ; path ; parent } ->
    let buf = Buffer.create 74 in
    Buffer.add_char buf (Char.chr (List.length path)) ;
    Buffer.add_string buf Cstruct.(sub parent 0 4 |> to_string) ;
    let cs = Cstruct.create 4 in
    Cstruct.BE.set_uint32 cs 0 (match path with [] -> 0l  | i :: _ -> i) ;
    Buffer.add_string buf (Cstruct.to_string cs) ;
    Buffer.add_string buf (Cstruct.to_string c) ;
    begin match k with
      | Sk sk ->
        let cs = Cstruct.create 33 in
        Secret.write cs.buffer ~pos:1 sk ;
        Buffer.add_string buf (Cstruct.to_string cs)
      | Pk pk ->
        let pk = Public.to_bytes ~compress:true ctx pk in
        Buffer.add_string buf Cstruct.(of_bigarray pk |> to_string)
    end ;
    Buffer.contents buf

  let to_cstruct key =
    Cstruct.of_string (to_bytes key)
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
