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

(* Record Types *)
(* ------------ *)

type 't record_msg =
  { valid : bool [@key 1]
  ; mutable ephemeralKey : 't [@key 2]
  ; mutable initEphemeralKey : 't [@key 3]
  ; ciphertext : 't [@key 4]
  ; mutable iv : 't [@key 5]
  ; tag : 't [@key 6]
  ; preKeyId : int [@key 7]
  }
[@@deriving protobuf]

type 't record_keypair =
  { mutable priv : 't [@key 1]
  ; mutable pub : 't [@key 2]
  }
[@@deriving protobuf]

type 't record_them =
  { mutable signedPreKey : 't [@key 1]
  ; mutable signedPreKeySignature : 't [@key 2]
  ; mutable identityKey : 't [@key 3]
  ; mutable identityDHKey : 't [@key 4]
  ; mutable myEphemeralKeyP0 : 't record_keypair [@key 5]
  ; mutable myEphemeralKeyP1 : 't record_keypair [@key 6]
  ; mutable ephemeralKey : 't [@key 7]
  ; mutable myPreKey : 't record_keypair [@key 8]
  ; mutable preKey : 't [@key 9]
  ; preKeyId : int [@key 10]
  ; recvKeys : 't array [@key 11]
  ; sendKeys : 't array [@key 12]
  ; mutable shared : 't [@key 13]
  ; established : bool [@key 14]
  }
[@@deriving protobuf]

type 't record_sendoutput =
  { mutable them : 't record_them [@key 1]
  ; mutable output : 't record_msg [@key 2]
  }
[@@deriving protobuf]

type 't record_recvoutput =
  { mutable them : 't record_them [@key 1]
  ; mutable output : 't record_msg [@key 2]
  ; plaintext : 't [@key 3]
  }
[@@deriving protobuf]

type 't record_ratchetsendkeys =
  { sendKeys : 't array [@key 1]
  ; kENC : 't [@key 2]
  }
[@@deriving protobuf]

type 't record_ratchetrecvkeys =
  { recvKeys : 't array [@key 1]
  ; kENC : 't [@key 2]
  }
[@@deriving protobuf]

(* Module Types *)
(* ------------ *)

(* Note: The full signature does not need to be specified. Instead, only the parts that we want users to see should be part of this signature. *)

(** The KBB2017 protocol *)
module type PROTOCOL = sig
  (** The type that will be used to represent contiguous bytes in the protocol; typically Bytes.t or Cstruct.t *)
  type t

  (** An internal type representing a decrypted AES message *)
  type t_aes_decrypted

  (** Entry point for sending and receiving messages while maintaining a KBB2017 session. *)
  module TOPLEVEL : sig
    val newSession :
         t record_keypair
      -> t record_keypair
      -> t
      -> t
      -> t
      -> t
      -> t
      -> int
      -> t record_them
    (** [newSession mySignedPreKey myPreKey theirIdentityKeyPub theirIdentityDHKeyPub theirSignedPreKeyPub theirSignedPreKeySignature theirPreKeyPub preKeyId] creates a KBB2017 session
        from "my" prekeys and "their" public information while consuming a one-time pre-key [preKeyId].

        Using nomenclature from the KBB2017 paper, the expectation is that the "their" party has published their long-term
        Diffie-Hellman public key [theirIdentityDHKeyPub] and a set of ephemeral Diffie-Hellman public keys ([theirSignedPreKeyPub] and [theirPreKeyPub])
        called "pre-keys" to a common server or other shared location. The pre-keys include both signed pre-keys [mySignedPreKey], which can be reused for
        some period of time, and non-signed, one-time pre-keys ([myPreKey] and their [preKeyId]), which are fresh at each session.
     *)

    val send : t record_keypair -> t record_them -> t -> t record_sendoutput
    (** [send myIdentityKey them plaintext] sends a [plaintext] message to [them] authenticated by [myIdentityKey].
      *)

    val recv :
         t record_keypair
      -> t record_keypair
      -> t record_them
      -> t record_msg
      -> t record_recvoutput
    (** [recv myIdentityKey mySignedPreKey them msg] receives an encrypted message [msg] that was ostensibly obtained
        from [them] and verifies that it was sent to the session both established with a pre-key signed with [mySignedPreKey] and
        authenticated with [myIdentityKey].

        @return a {!record_recvoutput} that, if and only if [return_value.output.valid] is true, will have valid plaintext
      *)
  end

  (** A private or public key *)
  module Type_key : sig
    val construct : unit -> t
    (** Create a key *)

    val toBitstring : t -> t

    val fromBitstring : t -> t

    val xassert : t -> t
    (** Verify that the object is a valid key object.
        @raise Invalid_argument when the object is not a valid key object. *)

    val clone : t -> t
  end

  (** An initialization vector *)
  module Type_iv : sig
    val construct : unit -> t
    (** Create an initialization vector *)

    val toBitstring : t -> t

    val fromBitstring : t -> t

    val xassert : t -> t
    (** Verify that the object is a valid iv object.
        @raise Invalid_argument when the object is not a valid iv object. *)
  end

  (** A message *)
  module Type_msg : sig
    val construct : unit -> t record_msg
    (** Create a message *)

    val xassert : t record_msg -> t record_msg
    (** Verify that the object is a valid record_msg object.
        @raise Invalid_argument when the object is not a valid record_msg object. *)
  end

  (** A public and private keypair *)
  module Type_keypair : sig
    val construct : unit -> t record_keypair
    (** Create a keypair *)

    val xassert : t record_keypair -> t record_keypair
    (** Verify that the object is a valid record_keypair object.
        @raise Invalid_argument when the object is not a valid record_keypair object. *)

    val clone : t record_keypair -> t record_keypair
  end

  (** A session tracking the other party in a conversation *)
  module Type_them : sig
    val construct : unit -> t record_them
    (** Create a record of a session with another party *)

    val xassert : t record_them -> t record_them
    (** Verify that the object is a valid record_them object.
        @raise Invalid_argument when the object is not a valid record_them object. *)
  end

  (** The updates to your record of the other party {e that should be persisted} after sending a message *)
  module Type_sendoutput : sig
    val construct : unit -> t record_sendoutput
    (** Create a sendoutput object *)

    val xassert : t record_sendoutput -> t record_sendoutput
    (** Verify that the object is a valid sendoutput object.
        @raise Invalid_argument when the object is not a valid sendoutput object. *)
  end

  (** The updates to your record of the other party {e that should be persisted} after receiving a message *)
  module Type_recvoutput : sig
    val construct : unit -> t record_recvoutput
    (** Create a recvoutput object *)

    val xassert : t record_recvoutput -> t record_recvoutput
    (** Verify that the object is a valid recvoutput object.
        @raise Invalid_argument when the object is not a valid recvoutput object. *)
  end

  (** Cryptographic helpers used in KBB2017 *)
  module UTIL : sig
    val xHKDF : t -> t -> t -> t array
    (** [xHKDF ikm salt info] extracts a pseudo-random key from the input keying material [ikm] and a [salt]
        and expands it to derive two derivative keys from an optional application- and context-specific [info]
        using the
        {{:https://tools.ietf.org/html/rfc5869} RFC 5869 HMAC-based Extract-and-Expand Key Derivation Function}
        construction.

        @return an array [ [| k0; k1 |] ] where [k0] is the derivative key from the first expansion
          and [k1] is the derivative key from the second expansion
      *)

    val xQDHInit : t -> t -> t -> t -> t -> t
    (** [xQDHInit myIdentityKeyPriv myInitEphemeralKeyPriv theirIdentityKeyPub theirSignedPreKeyPub theirPreKeyPub] performs the
        {{:https://signal.org/docs/specifications/x3dh/#sending-the-initial-message} quad Diffie-Helman construction for
        "Sending the initial message" of the X3DH Key Agreement Protocol}.
      *)

    val xQDHResp : t -> t -> t -> t -> t -> t
    (** [xQDHInit myIdentityKeyPriv mySignedPreKeyPriv myPreKeyPriv theirIdentityKeyPub theirEphemeralKeyPub] performs the
        {{:https://signal.org/docs/specifications/x3dh/#receiving-the-initial-message} quad Diffie-Helman construction
        for "Receiving the initial message" of the X3DH Key Agreement Protocol}.
      *)

    val newIdentityKey : t -> t record_keypair
    (** [newIdentityKey id] creates a key pair with a randomly initialized 32 byte private key and its 32 bytes ED25519 public key.

      [id] will be ignored for a true random number generator. But [id] may be used for mock random
      number generators or pseudo random generators to provide repeatability.
      *)

    val newKeyPair : t -> t record_keypair
    (** [newKeyPair id] creates a key pair with a randomly initialized 32 byte private key and its 32 byte DH25519 (aka x25519) public key.

      [id] will be ignored for a true random number generator. But [id] may be used for mock random
      number generators or pseudo random generators to provide repeatability.
     *)

    val getDHPublicKey : t -> t
    (** [getDHPublicKey priv] gives the DH25519 public key corresponding to the private key [priv] *)
  end

  (** Double Ratchet algorithm *)
  module RATCHET : sig
    val deriveSendKeys : t record_them -> t -> t record_ratchetsendkeys

    val deriveRecvKeys : t -> t record_them -> t -> t record_ratchetrecvkeys

    val tryDecrypt :
         t record_keypair
      -> t record_keypair
      -> t record_them
      -> t record_msg
      -> t_aes_decrypted
  end

  (** KBB2017 session update logic *)
  module HANDLE : sig
    val xAKENeeded :
      t record_keypair -> t record_keypair -> t record_them -> t record_them

    val xAKEInit :
         t record_keypair
      -> t record_keypair
      -> t record_them
      -> t record_msg
      -> t record_them

    val sending :
      t record_keypair -> t record_them -> t -> t -> t record_sendoutput

    val receiving :
      t record_keypair -> t record_them -> t record_msg -> t record_recvoutput
  end
end

module type PROTOCOLFUNCTOR = functor (ProScript : Dirsp_proscript.S) ->
  PROTOCOL with type t = ProScript.t

module type PROTOCOLMODULE = sig
  module Make : PROTOCOLFUNCTOR
end
