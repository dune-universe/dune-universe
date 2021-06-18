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
(** Providing the signatures of cryptographic primitives needed by verified ProScript code.

There are other libraries, especially [dirsp-proscript-mirage], that provide
implementations for the cryptographic primitives.
*)

exception Crypto_failure of string

exception Encoding_failure of string

(** The [ProScript.encoding] interface for conversions back and forth between various encodings like hexadecimal. *)
module type PROSCRIPT_ENCODING = sig
  (** The type that will be used to represent contiguous bytes; typically either Bytes.t or Cstruct.t *)
  type t

  (** The type that will be used to represent a single byte *)
  type t_elem

  val b2h : t_elem -> t
  (** [b2h c] converts a byte [c] into a 2-byte buffer filled with the hexadecimal octet of [c].

  @raise Encoding_failure for any failure *)

  val hexStringTo32ByteArray : t -> t
  (** [hexStringTo32ByteArray hex] takes a buffer [hex] of a hexadecimal text of least length 64 and then converts it to the 32 bytes the hexadecimal octets represents.

  This function signature looks odd because it doesn't take in a string. Since ProScript -> OCaml auto-translation
  must convert all strings immediately into buffers because the translator can't deduce when it is appropriate
  to use strings or buffers, your code needs to do use buffers consistently as well.

  @raise Encoding_failure for any failure *)

  val hexStringTo12ByteArray : t -> t
  (** [hexStringTo12ByteArray hex] takes a buffer [hex] of a hexadecimal text of at least length 24 and then converts it to the 12 bytes the hexadecimal octets represents.

  This function signature looks odd because it doesn't take in a string. Since ProScript -> OCaml auto-translation
  must convert all strings immediately into buffers because the translator can't deduce when it is appropriate
  to use strings or buffers, your code needs to do use buffers consistently as well.

  @raise Encoding_failure for any failure *)
end

(** The [ProScript.crypto.ED25519] interface for the elliptic curve 25519 DSA.

  DSA is for computing and verifying digital signatures, in contrast to
  Diffie-Hellman which is for mutually exchanging keys. Use {!Proscript_Crypto.xDH25519}
  for Diffie-Hellman key exchange.
*)
module type PROSCRIPT_CRYPTO_ED25519 = sig
  (** The type that will be used to represent contiguous bytes; typically either Bytes.t or Cstruct.t *)
  type t

  val publicKey : t -> t
  (** [publicKey sk] computes the 32-byte ED25519 public key for the secret key [sk].

      @raise Crypto_failure when the secret key is not 32 bytes *)

  val checkValid : t -> t -> t -> bool
  (** [checkValid s m pk] checks the message [m] for a valid signature
      [s] signed by the private key that corresponds to the raw public key [pk].

      @raise Crypto_failure when the signature is not 64 bytes or the public key is not 32 bytes *)

  val signature : t -> t -> t -> t
  (** [signature m sk pk] calculates the ED25519 signature of [m] with a raw
      32-byte secret key [sk] and its corresponding raw 32-byte public key [pk] created with [(]{!publicKey}[ sk].

      {b WARNING}: It is {e incorrect} to create a ED25519 signature using a public key [pk] that is not
      derived from the secret key [sk]. You provide the [pk] simply because doing so is the ProScript API,
      and because it can be a performance win to not recompute {!publicKey} each time you do a signature.

      1. You need to hex encode the message.

      Checks may or may not be performed to verify you gave the correct public key. Implementations have the
      freedom to ignore whatever public key you provide (ex. [dirsp-proscript-mirage] will recompute the public
      key and ignore your public key).

      @raise Crypto_failure when the secret or public key is not 32 bytes *)
end

(** The [ProScript.crypto] interface for core cryptographic primitives like SHA-256. *)
module type PROSCRIPT_CRYPTO = sig
  (** The type that will be used to represent contiguous bytes; typically either Bytes.t or Cstruct.t *)
  type t

  (** Encrypted AES data *)
  type aes_encrypted =
    { ciphertext : t
    ; tag : t
    }

  (** Decrypted AES data *)
  type aes_decrypted =
    { plaintext : t
    ; valid : bool
    }

  val random12Bytes : t -> t
  (** [random12Bytes id] creates a 12-byte random buffer with [id] providing a random generation hint.

      [id] will be ignored for a true random number generator. But [id] may be used for mock random
      number generators or pseudo random generators to provide repeatability.

      @raise Crypto_failure for a failure to generate random numbers *)

  val random32Bytes : t -> t
  (** [random32Bytes id] creates a 32-byte random buffer with [id] providing a random generation hint.

      [id] will be ignored for a true random number generator. But [id] may be used for mock random
      number generators or pseudo random generators to provide repeatability.

      @raise Crypto_failure for a failure to generate random numbers *)

  val xDH25519 : t -> t -> t
  (** [xDH25519 scalar base], commonly known as X25519, creates a public key or shared secret using the product
      of the private key [scalar] and the base point or public key [base] on an elliptic curve.

      @param scalar Private key in a 32-byte buffer

		  @param base Base point (or public key)

		  @return Public key (or shared secret if [base] was a public key) in a 32-byte buffer

      @raise Crypto_failure when the private key or base point are not both 32 bytes long *)

  val xAESGCMEncrypt : t -> t -> t -> t -> aes_encrypted
  (** [xAESGCMEncrypt k iv m aad] encrypts the message [m] with the symmetric key [k] and an initialization vector [iv] and any additional authenticated data [aad]
      using the AES-GCM algorithm.

      @raise Crypto_failure when the symmetric key is not a correct length for AES or when the initialization vector is not a correct length for AES-GCM *)

  val xAESGCMDecrypt : t -> t -> aes_encrypted -> t -> aes_decrypted
  (** [xAESGCMDecrypt k iv m aad] decrypts the message [m] with the symmetric key [k] and an initialization vector [iv] and any additional authenticated data [aad]
      using the AES-GCM algorithm.

      Check the validity of the result by looking at the returned {!aes_decrypted.valid}.

      @raise Crypto_failure when the symmetric key is not a correct length for AES or when the initialization vector is not a correct length for AES-GCM *)

  val xSHA256 : t -> t
  (** [xSHA256 m] constructs the SHA-256 hexadecimal digest of message [m].

      {b Be aware} that this function gives the ASCII bytes of the hexdump of the SHA-256 digest,
      not the raw SHA-256 digest bytes. The bytes look like the output of the first column
      from the UNIX program "sha256sum".
    *)

  val xSHA512 : t -> t
  (** [xSHA512 m] constructs the SHA-512 hexadecimal digest of message [m].

      {b Be aware} that this function gives the ASCII bytes of the hexdump of the SHA-512 digest,
      not the raw SHA-512 digest bytes. The bytes look like the output of the first column
      from the UNIX program "sha512sum".
    *)

  val xHMACSHA256 : t -> t -> t
  (** [xHMACSHA256 k m] constructs the authentication code for message [m] under the secret key [k]
      using standard HMAC construction over SHA-256, commonly known as the HMAC-SHA-256 algorithm. *)

  module ED25519 : PROSCRIPT_CRYPTO_ED25519 with type t = t
end

(** Signature of cryptographic primitives and low-level functions needed by verified ProScript code. *)
module type S = sig
  (** The type that will be used to represent contiguous bytes; typically either Bytes.t or Cstruct.t *)
  type t

  (** The type that will be used to represent a single byte *)
  type t_elem

  val equal : t -> t -> bool
  (** [equal t_a t_b] is true if and only if the buffers [t_a] and [t_b] contain the same bytes *)

  val concat : t list -> t
  (** [concat b_lst] take a list of buffers [b_lst], concatenates them together into one buffer *)

  val of_string : string -> t
  (** [of_string s] converts a string [s] into a buffer *)

  val of_elem_list : t_elem list -> t
  (** [of_elem_list elem_lst] concatenates a list of byte elements [elem_lst] into a buffer *)

  val of_cstruct : Cstruct.t -> t
  (** [of_cstruct cs] converts a Cstruct [cs] into a buffer *)

  val to_bytes : t -> Bytes.t
  (** [to_bytes b] converts a buffer [b] into Bytes *)

  val elem_at : t -> int -> t_elem
  (** [elem_at b i] gives the byte element at the zero-based position [i] of buffer [b] *)

  val char_of_elem : t_elem -> char
  (** [char_of_elem e] converts a byte element [e] to a character (a byte) *)

  val elem_of_char : char -> t_elem
  (** [elem_of_char c] convert a byte [c] into a buffer element *)

  val hexdump : t -> unit
  (** [hexdump_pp b] gives a thunk that pretty-prints the buffer [b] to standard output *)

  val hexdump_pp : Format.formatter -> t -> unit
  (** [hexdump_pp fmt b] gives a thunk that pretty-prints the buffer [b] to the formatter [fmt] *)

  val t_from_protobuf : Protobuf.Decoder.t -> t
  (** [t_from_protobuf d] reads the next value from the protobuf decoder [d] and converts it into a buffer *)

  val t_to_protobuf : t -> Protobuf.Encoder.t -> unit
  (** [t_to_protobuf b e] creates a thunk which will encode the buffer [b] with the protobuf encoder [e] when called *)

  (** Conversions back and forth between various encodings like hexadecimal. *)
  module Encoding : PROSCRIPT_ENCODING with type t = t and type t_elem = t_elem

  (** Core cryptographic primitives like SHA-256. *)
  module Crypto : PROSCRIPT_CRYPTO with type t = t
end
