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
module type PROSCRIPT_ENCODING = sig
  type t

  type t_elem

  val b2h : t_elem -> t

  val hexStringTo32ByteArray : t -> t

  val hexStringTo12ByteArray : t -> t
end

module type PROSCRIPT_CRYPTO_ED25519 = sig
  type t

  val publicKey : t -> t

  val checkValid : t -> t -> t -> bool

  val signature : t -> t -> t -> t
end

module type PROSCRIPT_CRYPTO = sig
  type t

  type aes_encrypted =
    { ciphertext : t
    ; tag : t
    }

  type aes_decrypted =
    { plaintext : t
    ; valid : bool
    }

  val random12Bytes : t -> t

  val random32Bytes : t -> t

  val xDH25519 : t -> t -> t

  val xAESGCMEncrypt : t -> t -> t -> t -> aes_encrypted

  val xAESGCMDecrypt : t -> t -> aes_encrypted -> t -> aes_decrypted

  val xSHA256 : t -> t

  val xSHA512 : t -> t

  val xHMACSHA256 : t -> t -> t

  module ED25519 : PROSCRIPT_CRYPTO_ED25519 with type t = t
end

module type S = sig
  type t

  type t_elem

  val equal : t -> t -> bool

  val concat : t list -> t

  val of_string : string -> t

  val of_elem_list : t_elem list -> t

  val of_cstruct : Cstruct.t -> t

  val to_bytes : t -> Bytes.t

  val elem_at : t -> int -> t_elem

  val char_of_elem : t_elem -> char

  val elem_of_char : char -> t_elem

  val hexdump : t -> unit

  val hexdump_pp : Format.formatter -> t -> unit

  val t_from_protobuf : Protobuf.Decoder.t -> t

  val t_to_protobuf : t -> Protobuf.Encoder.t -> unit

  module Encoding : PROSCRIPT_ENCODING with type t = t and type t_elem = t_elem

  module Crypto : PROSCRIPT_CRYPTO with type t = t
end

exception Crypto_failure of string

exception Encoding_failure of string
