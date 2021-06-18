(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
include Dirsp_proscript

module type CUSTOM_OPTIONS = sig
  val random_bytes : int -> string -> Bytes.t
end

module AES_GCM = Mirage_crypto.Cipher_block.AES.GCM

let sz_DH25519_key = 32

let sz_EDH25519_key = 32

let sz_EDH25519_signature = 64

let hexcode c =
  match c with
  | '0' -> Ok 0
  | '1' -> Ok 1
  | '2' -> Ok 2
  | '3' -> Ok 3
  | '4' -> Ok 4
  | '5' -> Ok 5
  | '6' -> Ok 6
  | '7' -> Ok 7
  | '8' -> Ok 8
  | '9' -> Ok 9
  | 'a' | 'A' -> Ok 10
  | 'b' | 'B' -> Ok 11
  | 'c' | 'C' -> Ok 12
  | 'd' | 'D' -> Ok 13
  | 'e' | 'E' -> Ok 14
  | 'f' | 'F' -> Ok 15
  | _ -> Error "Not a hexadecimal digit"


let hexchars c =
  let ch = int_of_char c in
  let t = "0123456789abcdef" in
  let a = (ch lsr 4) land 15 in
  let b = ch land 15 in
  (String.get t a, String.get t b)


module type S = sig
  type t

  type t_elem

  val equal : t -> t -> bool

  val concat : t list -> t

  val of_string : string -> t

  val of_elem_list : t_elem list -> t

  val of_cstruct : Cstruct.t -> t

  val to_bytes : t -> Bytes.t

  val elem_of_char : char -> t_elem

  val char_of_elem : t_elem -> char

  val elem_at : t -> int -> t_elem

  val hexdump : t -> unit

  val hexdump_pp : Format.formatter -> t -> unit

  val t_from_protobuf : Protobuf.Decoder.t -> t

  val t_to_protobuf : t -> Protobuf.Encoder.t -> unit

  module Encoding : PROSCRIPT_ENCODING with type t = t and type t_elem = t_elem

  module Crypto : PROSCRIPT_CRYPTO with type t = t
end

module CustomizedMake (Opts : CUSTOM_OPTIONS) : S = struct
  type t = Bytes.t

  type t_elem = char

  let buffer_to_elem_array b =
    let n = Bytes.length b in
    Array.init n (fun i -> Bytes.get b i)


  let elem_array_to_buffer a =
    let n = Array.length a in
    Bytes.init n (fun i -> Array.get a i)


  let equal a b = Bytes.equal a b

  let concat l =
    elem_array_to_buffer
      (Array.concat
         (List.map
            (function
              | i -> buffer_to_elem_array i )
            l ) )


  let of_string s =
    elem_array_to_buffer
      (Array.init (String.length s) (function i -> String.get s i))


  let of_elem_list l = elem_array_to_buffer (Array.of_list l)

  let to_bytes b = b

  let of_cstruct cs =
    let sz = Cstruct.length cs in
    let buffer = Bytes.create sz in
    Cstruct.blit_to_bytes cs 0 buffer 0 sz ;
    buffer


  let elem_of_char c = c

  let char_of_elem e = e

  let elem_at b i = Array.get (buffer_to_elem_array b) i

  let hexdump b =
    let bs = b in
    Cstruct.hexdump (Cstruct.of_bytes ~off:0 ~len:(Bytes.length bs) bs)


  let hexdump_pp fmter b =
    let bs = b in
    Cstruct.hexdump_pp fmter (Cstruct.of_bytes ~off:0 ~len:(Bytes.length bs) bs)


  let t_from_protobuf = Protobuf.Decoder.bytes

  let t_to_protobuf = Protobuf.Encoder.bytes

  module Encoding = struct
    type t = Bytes.t

    type t_elem = char

    (* ------------------------ *)
    (* Private helper functions *)
    (* ------------------------ *)

    let h_faile msg = raise (Encoding_failure msg)

    let h_hexoctet_to_int ((c1, c2) : char * char) =
      let code1 = hexcode c1 in
      let code2 = hexcode c2 in
      match (code1, code2) with
      | Ok cd1, Ok cd2 -> Ok ((cd1 * 16) + cd2)
      | _ -> Error "Not a hexadecimal octet"


    let h_init_byte_from_hex hexbs i =
      let c1 = Bytes.get hexbs (i * 2) in
      let c2 = Bytes.get hexbs ((i * 2) + 1) in
      let octet = h_hexoctet_to_int (c1, c2) in
      match octet with
      | Ok v -> char_of_int v
      | Error s -> h_faile s


    (* ---------- *)
    (* Public API *)
    (* ---------- *)

    let b2h c =
      let c1, c2 = hexchars c in
      of_elem_list [ c1; c2 ]


    let hexStringToByteArray hex sz =
      if Bytes.length hex >= sz * 2
      then ()
      else
        h_faile
          ("Not text with at least " ^ string_of_int (sz * 2) ^ " hex digits") ;
      let bs = Bytes.init sz (h_init_byte_from_hex hex) in
      bs


    let hexStringTo32ByteArray hex = hexStringToByteArray hex 32

    let hexStringTo12ByteArray hex = hexStringToByteArray hex 12
  end

  module Crypto = struct
    type t = Bytes.t

    type aes_encrypted =
      { ciphertext : t
      ; tag : t
      }

    type aes_decrypted =
      { plaintext : t
      ; valid : bool
      }

    (* ------------------------ *)
    (* Private helper functions *)
    (* ------------------------ *)

    let h_failc msg = raise (Crypto_failure msg)

    let h_check_crypto_sz name buf expected_sz =
      let actual_sz = Bytes.length buf in
      if actual_sz = expected_sz
      then ()
      else
        h_failc
          ( "Bad "
          ^ name
          ^ " size. Expected "
          ^ string_of_int expected_sz
          ^ " not "
          ^ string_of_int actual_sz )


    let h_randomBytes sz (id : t) = Opts.random_bytes sz (Bytes.to_string id)

    let h_cstruct_to_hexbuffer cs : t =
      let octets = Cstruct.length cs in
      let bs = Bytes.create (octets * 2) in
      for i = 0 to octets - 1 do
        let c = Cstruct.get_char cs i in
        let c1, c2 = hexchars c in
        Bytes.set bs (i * 2) c1 ;
        Bytes.set bs ((i * 2) + 1) c2
      done ;
      bs


    (* ---------- *)
    (* Public API *)
    (* ---------- *)

    let random12Bytes = h_randomBytes 12

    let random32Bytes = h_randomBytes 32

    let xDH25519 scalar base =
      (* preconditions *)
      h_check_crypto_sz "DH25519 scalar" scalar sz_DH25519_key ;
      h_check_crypto_sz "DH25519 base" base sz_DH25519_key ;

      (* secret *)
      let secret_cs = Cstruct.create sz_DH25519_key in
      Cstruct.blit_from_bytes scalar 0 secret_cs 0 sz_DH25519_key ;
      let secret_of_cs_result =
        Mirage_crypto_ec.X25519.secret_of_cs secret_cs
      in

      (* base *)
      let base_cs = Cstruct.create sz_DH25519_key in
      Cstruct.blit_from_bytes base 0 base_cs 0 sz_DH25519_key ;

      (* action *)
      match secret_of_cs_result with
      | Error `Invalid_range -> h_failc "DH25519 secret_of_cs Invalid_range"
      | Error `Invalid_format -> h_failc "DH25519 secret_of_cs Invalid_format"
      | Error `Invalid_length -> h_failc "DH25519 secret_of_cs Invalid_length"
      | Error `Not_on_curve -> h_failc "DH25519 secret_of_cs Not_on_curve"
      | Error `At_infinity -> h_failc "DH25519 secret_of_cs At_infinity"
      | Error `Low_order -> h_failc "DH25519 secret_of_cs Low_order"
      | Ok (secret, _public_cs) ->
          let key_exchange_result =
            Mirage_crypto_ec.X25519.key_exchange secret base_cs
          in
          ( match key_exchange_result with
          | Error `Invalid_range -> h_failc "DH25519 key_exchange Invalid_range"
          | Error `Invalid_format ->
              h_failc "DH25519 key_exchange Invalid_format"
          | Error `Invalid_length ->
              h_failc "DH25519 key_exchange Invalid_length"
          | Error `Not_on_curve -> h_failc "DH25519 key_exchange Not_on_curve"
          | Error `At_infinity -> h_failc "DH25519 key_exchange At_infinity"
          | Error `Low_order -> h_failc "DH25519 key_exchange Low_order"
          | Ok _shared_secret -> of_cstruct _shared_secret )


    let xAESGCMEncrypt k iv m aad =
      (* key *)
      let key_sz = Bytes.length k in
      let key_cs = Cstruct.create key_sz in
      Cstruct.blit_from_bytes k 0 key_cs 0 key_sz ;
      let key = AES_GCM.of_secret key_cs in

      (* nonce *)
      let nonce_sz = Bytes.length iv in
      let nonce = Cstruct.create nonce_sz in
      Cstruct.blit_from_bytes iv 0 nonce 0 nonce_sz ;

      (* adata *)
      let adata_sz = Bytes.length aad in
      let adata = Cstruct.create adata_sz in
      Cstruct.blit_from_bytes aad 0 adata 0 adata_sz ;

      (* msg *)
      let msg_sz = Bytes.length m in
      let msg = Cstruct.create msg_sz in
      Cstruct.blit_from_bytes m 0 msg 0 msg_sz ;

      (* action *)
      try
        (*
          WARNING
          -------

          Mirage bundles the encrypted data and authentication into
          'encrypt_result_cs' and does not provide an API to split them
          apart.

          Cf. https://mirage.github.io/mirage-crypto/doc/mirage-crypto/Mirage_crypto/Cipher_block/AES/GCM/index.html

          So ...

          1. Rely on their documentation which says "appends an authentication tag"
          2. Rely on unit testing to make sure documentation reflects reality
          *)
        let encrypt_result_cs =
          AES_GCM.authenticate_encrypt ~key ~nonce ~adata msg
        in
        let ciphertext_cs, tag_cs =
          let result_len = Cstruct.length encrypt_result_cs in
          let pretag_len = result_len - AES_GCM.tag_size in
          let dst_msg = Cstruct.create pretag_len in
          Cstruct.blit encrypt_result_cs 0 dst_msg 0 pretag_len ;
          let dst_tag = Cstruct.create AES_GCM.tag_size in
          Cstruct.blit encrypt_result_cs pretag_len dst_tag 0 AES_GCM.tag_size ;
          (dst_msg, dst_tag)
        in
        { ciphertext = of_cstruct ciphertext_cs; tag = of_cstruct tag_cs }
      with
      | Invalid_argument e -> h_failc ("xAESGCMEncrypt " ^ e)


    let xAESGCMDecrypt k iv (m : aes_encrypted) aad =
      (* key *)
      let key_sz = Bytes.length k in
      let key_cs = Cstruct.create key_sz in
      Cstruct.blit_from_bytes k 0 key_cs 0 key_sz ;
      let key = AES_GCM.of_secret key_cs in

      (* nonce *)
      let nonce_sz = Bytes.length iv in
      let nonce = Cstruct.create nonce_sz in
      Cstruct.blit_from_bytes iv 0 nonce 0 nonce_sz ;

      (* adata *)
      let adata_sz = Bytes.length aad in
      let adata = Cstruct.create adata_sz in
      Cstruct.blit_from_bytes aad 0 adata 0 adata_sz ;

      (* msg = ciphertext || tag *)
      let { ciphertext; tag } = m in
      let ciphertext_sz = Bytes.length ciphertext in
      let tag_sz = Bytes.length tag in
      let msg_sz = ciphertext_sz + tag_sz in
      let msg = Cstruct.create msg_sz in
      Cstruct.blit_from_bytes ciphertext 0 msg 0 ciphertext_sz ;
      Cstruct.blit_from_bytes tag 0 msg ciphertext_sz tag_sz ;

      (* action *)
      try
        let decrypt_result_cs_opt =
          AES_GCM.authenticate_decrypt ~key ~nonce ~adata msg
        in
        match decrypt_result_cs_opt with
        | None -> { valid = false; plaintext = of_elem_list [] }
        | Some plaintext_cs ->
            { valid = true; plaintext = of_cstruct plaintext_cs }
      with
      | Invalid_argument e -> h_failc ("xAESGCMDecrypt " ^ e)


    let xSHA256 m =
      (* msg *)
      let msg_sz = Bytes.length m in
      let msg_cs = Cstruct.create msg_sz in
      Cstruct.blit_from_bytes m 0 msg_cs 0 msg_sz ;

      (* action *)
      let digest = Mirage_crypto.Hash.SHA256.digest msg_cs in

      (* result must be hex encoded per ProScript API *)
      h_cstruct_to_hexbuffer digest


    let xSHA512 m =
      (* msg *)
      let msg_sz = Bytes.length m in
      let msg_cs = Cstruct.create msg_sz in
      Cstruct.blit_from_bytes m 0 msg_cs 0 msg_sz ;

      (* action *)
      let digest = Mirage_crypto.Hash.SHA512.digest msg_cs in

      (* result must be hex encoded per ProScript API *)
      h_cstruct_to_hexbuffer digest


    let xHMACSHA256 k m =
      (*
        Implementation Warning
        ----------------------
        HMAC can use any size of key. It will left pad if the key is too short, and it will hash if it is too long.
        So do _not_ insert a check like:
          h_check_crypto_sz "xHMACSHA256 k" k sz_HMACSHA256_digest ;
      *)

      (* key *)
      let key_sz = Bytes.length k in
      let key_cs = Cstruct.create key_sz in
      Cstruct.blit_from_bytes k 0 key_cs 0 key_sz ;

      (* msg *)
      let msg_sz = Bytes.length m in
      let msg_cs = Cstruct.create msg_sz in
      Cstruct.blit_from_bytes m 0 msg_cs 0 msg_sz ;

      (* action *)
      let mac = Mirage_crypto.Hash.SHA256.hmac ~key:key_cs msg_cs in
      of_cstruct mac


    module ED25519 = struct
      type t = Bytes.t

      let publicKey sk =
        (* preconditions *)
        h_check_crypto_sz "ED25519.publicKey sk" sk sz_EDH25519_key ;

        (* secret *)
        let secret_cs = Cstruct.create sz_EDH25519_key in
        Cstruct.blit_from_bytes sk 0 secret_cs 0 sz_EDH25519_key ;
        let priv_of_cstruct_result =
          Mirage_crypto_ec.Ed25519.priv_of_cstruct secret_cs
        in

        (* action *)
        match priv_of_cstruct_result with
        | Error `Invalid_range -> h_failc "ED25519 publicKey Invalid_range"
        | Error `Invalid_format -> h_failc "ED25519 publicKey Invalid_format"
        | Error `Invalid_length -> h_failc "ED25519 publicKey Invalid_length"
        | Error `Not_on_curve -> h_failc "ED25519 publicKey Not_on_curve"
        | Error `At_infinity -> h_failc "ED25519 publicKey At_infinity"
        | Error `Low_order -> h_failc "ED25519 publicKey Low_order"
        | Ok priv ->
            of_cstruct
              (Mirage_crypto_ec.Ed25519.pub_to_cstruct
                 (Mirage_crypto_ec.Ed25519.pub_of_priv priv) )


      let checkValid s m pk =
        (* preconditions *)
        h_check_crypto_sz "ED25519.checkValid s" s sz_EDH25519_signature ;
        h_check_crypto_sz "ED25519.checkValid pk" pk sz_EDH25519_key ;

        (* signature *)
        let signature = Cstruct.create sz_EDH25519_signature in
        Cstruct.blit_from_bytes s 0 signature 0 sz_EDH25519_signature ;

        (* msg *)
        let msg_sz = Bytes.length m in
        let msg = Cstruct.create msg_sz in
        Cstruct.blit_from_bytes m 0 msg 0 msg_sz ;

        (* public key *)
        let public_cs = Cstruct.create sz_EDH25519_key in
        Cstruct.blit_from_bytes pk 0 public_cs 0 sz_EDH25519_key ;
        let pub_of_cstruct_result =
          Mirage_crypto_ec.Ed25519.pub_of_cstruct public_cs
        in

        (* action *)
        match pub_of_cstruct_result with
        | Error `Invalid_range -> h_failc "ED25519 checkValid Invalid_range"
        | Error `Invalid_format -> h_failc "ED25519 checkValid Invalid_format"
        | Error `Invalid_length -> h_failc "ED25519 checkValid Invalid_length"
        | Error `Not_on_curve -> h_failc "ED25519 checkValid Not_on_curve"
        | Error `At_infinity -> h_failc "ED25519 checkValid At_infinity"
        | Error `Low_order -> h_failc "ED25519 checkValid Low_order"
        | Ok pub -> Mirage_crypto_ec.Ed25519.verify ~key:pub signature ~msg


      let signature m sk pk =
        (* preconditions *)
        h_check_crypto_sz "ED25519.signature sk" sk sz_EDH25519_key ;
        h_check_crypto_sz "ED25519.signature pk" pk sz_EDH25519_key ;

        (* msg *)
        let msg_sz = Bytes.length m in
        let msg = Cstruct.create msg_sz in
        Cstruct.blit_from_bytes m 0 msg 0 msg_sz ;

        (* secret key *)
        let key_cs = Cstruct.create sz_EDH25519_key in
        Cstruct.blit_from_bytes sk 0 key_cs 0 sz_EDH25519_key ;
        let priv_of_cstruct_result =
          Mirage_crypto_ec.Ed25519.priv_of_cstruct key_cs
        in

        (* action *)
        match priv_of_cstruct_result with
        | Error `Invalid_range -> h_failc "ED25519 signature Invalid_range"
        | Error `Invalid_format -> h_failc "ED25519 signature Invalid_format"
        | Error `Invalid_length -> h_failc "ED25519 signature Invalid_length"
        | Error `Not_on_curve -> h_failc "ED25519 signature Not_on_curve"
        | Error `At_infinity -> h_failc "ED25519 signature At_infinity"
        | Error `Low_order -> h_failc "ED25519 signature Low_order"
        | Ok key -> of_cstruct (Mirage_crypto_ec.Ed25519.sign ~key msg)
    end
  end
end

module DefaultOptions : CUSTOM_OPTIONS = struct
  let random_bytes sz _id =
    let buffer = Bytes.create sz in
    let rndbytes = Mirage_crypto_rng.generate sz in
    Cstruct.blit_to_bytes rndbytes 0 buffer 0 sz ;
    buffer
end

module Make () : S = CustomizedMake (DefaultOptions)
