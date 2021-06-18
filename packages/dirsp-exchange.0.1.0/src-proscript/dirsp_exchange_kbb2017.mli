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
(** For securing a conversation between two parties.

    {1 Summary}

    Dirsp is a short form for Diskuv Implementations of Research Security Protocols.
    The [dirsp-exchange-kbb2017] library implements the Kobeissi, Bhargavan and Blanchet 2017
    protocol that is a variant of the Signal Protocol v3 and has been verified
    using two complementary formal methodologies.
    It uses a [dirsp-proscript] library
    like [dirsp-proscript-mirage] to provide a runtime library of cryptographic primitives.

    Here is the simplest example for an exchange of messages (based on the
    {{:https://github.com/Inria-Prosecco/proscript-messaging/blob/ddb66e4934fefbbf10bbd393cabc4ecda5b75546/tl/secrecy-1-ab-twoway.pv} formal ProVerif secrecy-1-ab-twoway.pv proof test of secrecy}):

    {[
      (*  FIRST: Make some module shortcuts and initialize modules that need it *)
      module P       = Dirsp_proscript_mirage.Make()
      module C       = P.Crypto
      module ED25519 = P.Crypto.ED25519
      module E       = P.Encoding
      module K       = Dirsp_exchange_kbb2017.Make(P)
      module U       = K.UTIL
      module T       = K.TOPLEVEL
      module KEY     = K.Type_key
      module MSG     = K.Type_msg
      (* Initialize the random number generator; Mirage_crypto_rng_lwt works only on *nix *)
      let ()         = Mirage_crypto_rng_lwt.initialize ()
      ;;

      (*  SECOND: Let Alice and Bob create their own long-term key pairs.

          Alice keeps her private keys hidden, and Bob keeps his private
          keys hidden. As an implementor of this algorithm, you need to
          make sure there is a secure storage area for each person and place
          the private keys in that storage area.

          Note: The strings (ex. "alice-identity") are documentation hints, and are
          only meaningful during unit testing or proof testing.
      *)
      let aliceIdentityKey  = U.newIdentityKey (P.of_string "alice-identity")
      let aliceSignedPreKey = U.newKeyPair     (P.of_string "alice-signed-prekey")
      let bobIdentityKey    = U.newIdentityKey (P.of_string "bob-identity")
      let bobSignedPreKey   = U.newKeyPair     (P.of_string "bob-signed-prekey")
      ;;

      (* At any point you can see the contents of a key, message, etc. with `P.hexdump` *)
      P.hexdump aliceIdentityKey.pub
      ;;

      (*  THIRD: Alice and Bob exchange a set of long-term public keys.

          As an implementor, the exchange can be simply publishing the long-term keys
          once to a common location (ex. a server) and having each person download
          the other person's public keys.
       *)
      let aliceIdentityKeyPub        = aliceIdentityKey.pub
      let aliceIdentityDHKeyPub      = U.getDHPublicKey aliceIdentityKey.priv
      let aliceSignedPreKeyPub       = aliceSignedPreKey.pub
      let aliceSignedPreKeySignature = ED25519.signature (KEY.toBitstring aliceSignedPreKeyPub) aliceIdentityKey.priv aliceIdentityKeyPub
      let bobIdentityKeyPub          = bobIdentityKey.pub
      let bobIdentityDHKeyPub        = U.getDHPublicKey bobIdentityKey.priv
      let bobSignedPreKeyPub         = bobSignedPreKey.pub
      let bobSignedPreKeySignature   = ED25519.signature (KEY.toBitstring bobSignedPreKeyPub  ) bobIdentityKey.priv   bobIdentityKeyPub
      ;;

      (*  FOURTH: Alice and Bob create their own prekeys.

          Each prekey has an identifier [Id]. As an implementor of this algorithm, you need
          to make sure there is a secure storage area for each person's prekeys. This storage
          area needs to have lookup by an integer [Id].

          In this example, we create one prekey each person. As an implementator, you should create many.
       *)
      let alicePreKey   = U.newKeyPair     (P.of_string "alice-prekey")
      let bobPreKey     = U.newKeyPair     (P.of_string "bob-prekey")
      ;;

      (*  FIFTH: Alice consumes one of Bob's one-time prekeys. Bob consumes one of Alice's one-time prekeys.

          By "consume" we mean that once taken, the prekey can no longer be used. As an implementor, you
          need to have a common location (ex. a server) that lets:

          a) each person periodically post a large number of random prekeys
          b) each person consume prekeys that belongs to any other person

          {b CAUTION:} Be aware that "b" is an easy path to denial of service. Use standard anti-DOS
          techniques (ex. authentication, rate limiting on the consumer and the consumee) but inevitably
          that will not be enough. The solution from https://signal.org/docs/specifications/x3dh/#sending-the-initial-message
          is to fallback to an algorithm that uses only the signed prekey; KBB2017 however expects an infinite supply of
          prekeys. We suggest either letting each person post a "last resort" prekey that is used when all prekeys are
          exhausted -or- re-using the last prekey if all but the last prekey are exhausted.
      *)
      let alicePreKeyPub = alicePreKey.pub
      let alicePreKeyId  = 1 (* the Id of alicePreKey *)
      let bobPreKeyPub   = bobPreKey.pub
      let bobPreKeyId    = 1 (* the Id of bobPreKey   *)
      ;;

      (*  SIXTH: Alice establishes a session with Bob. *)
      let aliceSessionWithBob = T.newSession
        aliceSignedPreKey
        alicePreKey
        (KEY.toBitstring bobIdentityKeyPub)
        (KEY.toBitstring bobIdentityDHKeyPub)
        (KEY.toBitstring bobSignedPreKeyPub)
        bobSignedPreKeySignature
        (KEY.toBitstring bobPreKeyPub)
        bobPreKeyId
      ;;

      (*  SEVENTH: Alice sends a message to Bob and updates her session. *)
      let aliceToBobMsg1        = P.of_string "Hi Bob!"
      let aliceToBobSendOutput1 = T.send
        aliceIdentityKey
        aliceSessionWithBob
        aliceToBobMsg1
      ;;
      Format.printf "Did Alice send successfully? %b\n" aliceToBobSendOutput1.output.valid
      ;;
      (* Alice updates her session or she loses forward secrecy! *)
      let updatedAliceSessionWithBob = aliceToBobSendOutput1.them

      (*  EIGHTH: Bob establishes his own session with Alice. *)
      let bobSessionWithAlice = T.newSession
        bobSignedPreKey
        bobPreKey
        (KEY.toBitstring aliceIdentityKeyPub)
        (KEY.toBitstring aliceIdentityDHKeyPub)
        (KEY.toBitstring aliceSignedPreKeyPub)
        aliceSignedPreKeySignature
        (KEY.toBitstring alicePreKeyPub)
        alicePreKeyId
      ;;

      (*  NINTH: Bob receives the message from Alice and updates his session *)
      let bobFromAliceMsg2           = aliceToBobSendOutput1.output
      let bobFromAliceReceiveOutput2 = T.recv
        bobIdentityKey
        bobSignedPreKey
        bobSessionWithAlice
        bobFromAliceMsg2
      ;;
      Format.printf "Did Bob receive a message? %b\n"
        bobFromAliceReceiveOutput2.output.valid
      ;;
      Format.printf "Bob just received a new message: %s\n"
        (bobFromAliceReceiveOutput2.plaintext |> P.to_bytes |> Bytes.to_string)
      (* Bob updates his session or he loses forward secrecy! *)
      let updatedBobSessionWithAlice = bobFromAliceReceiveOutput2.them
    ]}

    The intent of the library is to provide software engineers with auditable
    source code that has some level of safety assurance (typically proofs) from security researchers.
    By "auditable" we mean the ability to justify every line of source code when undergoing an audit
    by a competent security engineer. No third-party vetting of the source code has been
    conducted (unless noted explicitly), and the original authors at Diskuv did not have security
    researchers or engineers on staff when the library was originally written.
    Contact {{:mailto:security@diskuv.com} security\@diskuv.com} to report any security issues, and feel
    free to shame publicly on {{:https://twitter.com/diskuv} Twitter \@diskuv} if Diskuv is not being responsive.

    You may be interested in
    {{:https://github.com/diskuv/dirsp-exchange#comparison_to_other_libraries}Comparison to Other Libraries}.

    See {!section:SourceCodeVerification} for help authenticating the source code history.

    {1 Institutions}

    The security research used in [dirsp-exchange] comes from various research and corporate
    institutions. Diskuv has no affiliation with any of the institutions below:

    {ol
    {- Signal (formerly Open Whisper Systems), a non-profit organization in the US}
    {- INRIA (National Institute for Research in Digital Science and Technology), a national research institute of France}
    }

    Please cite the institutions above rather than Diskuv. In fact, many academic licenses require
    you to credit the institution by name; see {!section:Licenses} further below.

    {1 Algorithms}

    {2 X3DH and Double Ratchet}

    X3DH was created by Open Whisper Systems and placed in the public domain in the publication
    {{:https://www.signal.org/docs/specifications/x3dh/} The X3DH Key Agreement Protocol}.
    That publication defines X3DH as "establish\[ing\] a shared secret key between two parties who
    mutually authenticate each other based on public keys. X3DH provides forward secrecy and
    cryptographic deniability."

    X3DH uses {{:https://www.signal.org/docs/specifications/doubleratchet/} The Double Ratchet Algorithm}
    as a building block.

    {{:https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md} Olm: A Cryptographic Ratchet}
    is a similar algorithm.

    {1:Licenses Licenses}

    [dirsp-exchange-kbb2017] is distributed under the
    {{:https://opensource.org/licenses/Apache-2.0} Apache-2.0} license.

    The [proscript-messaging] source files used in [Make] are distributed with the
    {{:https://github.com/Inria-Prosecco/proscript-messaging/blob/ddb66e4934fefbbf10bbd393cabc4ecda5b75546/ps2pv/LICENSE} following license}:

    {v
    Copyright (c) 2012-2013 Institut National de Recherche en Informatique et Automatique (Inria Paris-Rocquencourt)
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following
        disclaimer in the documentation and/or other materials provided
        with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
    WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
    OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
    IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation
    are those of the authors and should not be interpreted as representing
    official policies, either expressed or implied, of the authors.
    v}

    {1:SourceCodeVerification Source Code Verification}

    We primarily use an independent third party tool {{:https://github.com/codenotary/vcn} vChain CodeNotary} to establish
    an audit trail of what source code was placed when in this library. [vcn] lets us both create a trusted timestamp of commits
    and sign snapshots of third-party repositories, neither of which classical GPG git signing accomplish.
    Use {{:https://github.com/codenotary/vcn/blob/master/docs/cmd/vcn_authenticate.md} [vcn authenticate --signerID]} to validate git history.

    {ol
      {-
      The {{:https://github.com/Inria-Prosecco/proscript-messaging} proscript-messaging source code repository}
      has been forked at {{:https://github.com/diskuv/proscript-messaging} https://github.com/diskuv/proscript-messaging} for audit
      redundancy.
      {v
SignerID: 0xdaf31b40f750c92afc50e5750fac6a28da4cedc4
Code: https://github.com/Inria-Prosecco/proscript-messaging.git@ddb66e4
Hash: 3354e25ec0e92f6e99a1e26591c8c356d0c7b35b652539b624286c83e89a2a47
      v}
      }
    }

    {1 Unit Testing}

    If you need determinism, you can instantiate your own random number generation like the toy example below:

    {[
      module P = Dirsp_proscript_mirage.CustomizedMake(struct
        include Dirsp_proscript_mirage.DefaultOptions
        (** We use the first byte of MD5(id), and repeat it until we fill the requested size.
            {b DO NOT USE THIS IN PRODUCTION}; only use this in unit tests. *)
        let random_bytes sz id =
          let md5bytes = Digest.string id in
          let firstbyte = String.get md5bytes 0 in
          Bytes.init sz (fun i -> firstbyte)
      end)
    ]}

    @author Diskuv, Inc.
*)

(** Makes the Kobeissi, Bhargavan and Blanchet (KBB2017) verified security protocol that will delegate to a Proscript runtime library of cryptographic primitives.

    {1 Basics}

    {[
      module P       = Dirsp_proscript_mirage.Make()
      module C       = P.Crypto
      module ED25519 = P.Crypto.ED25519
      module E       = P.Encoding
      module K       = Dirsp_exchange_kbb2017.Make(P)
      module U       = K.UTIL
      module T       = K.TOPLEVEL
      module KEY     = K.Type_key
      module MSG     = K.Type_msg

      (* Alice sends a message to Bob *)
      let aliceSessionWithBob = T.newSession (* ... supply some keys you create with ED25519 and U ... *) ;;
      let aliceToBobSendOutput = T.send
        aliceIdentityKey
        aliceSessionWithBob
        (P.of_string "Hi Bob!")

      (* Now you can send the output "aliceToBobSendOutput" from Alice to Bob.
         Let's switch to Bob's computer. He gets notified using a notification
         library of your choosing and then does ...  *)

      let bobSessionWithAlice = T.newSession (* ... supply some keys ... *);;
      let bobFromAliceReceiveOutput = T.recv
        bobIdentityKey
        bobSignedPreKey
        bobSessionWithAlice
        theEncryptedMessageBobReceivedFromAlice
      assert (bobFromAliceReceiveOutput.output.valid)
      Format.printf "Bob just received a new message: %s\n"
        (bobFromAliceReceiveOutput.plaintext |> P.to_bytes |> Bytes.to_string)
    ]}

    {!Dirsp_exchange_kbb2017} has a more detailed example.

    {b You must not re-use the sessions!}. If you do, Alice and
    Bob's conversation will lose forward secrecy. Look at {!ManagingState}
    to see how to update the sessions.

    The functor [Make] creates an implementation of the X3DH and Double Rachet protocols.
    It makes use of a Javascript implementation of the Signal Protocol that was used to prove
    the soundness of a variant of the "Signal Protocol" (aka X3DH + Double Ratchet) in the paper
    by the Prosecco research group

    {v
    Nadim Kobeissi, Karthikeyan Bhargavan, Bruno Blanchet.
    Automated Verification for Secure Messaging Protocols
      and Their Implementations: A Symbolic and Computational Approach.
    2nd IEEE European Symposium on Security and Privacy , Apr 2017, Paris, France.
    pp.435 - 450, 2017, <https://www.ieee-security.org/TC/EuroSP2017/>.
    <10.1109/EuroSP.2017.38>. <hal-01575923>
    v}

    We call that paper’s algorithm the Kobeissi, Bhargavan and Blanchet (KBB2017) algorithm to both
    recognize the paper’s authors and to avoid the use of the trademark "Signal".
    Please be aware that the variant used in the Automated Verification paper deviates from the
    original Open Whisper System publications. [Make] corresponds to the variant
    in the Automated Verification paper.

    {1 Implementation}

    The Automated Verification paper has a
    {{:https://github.com/Inria-Prosecco/proscript-messaging} source code repository called proscript-messaging}.
    The authors of [dirsp-exchange] have a
    {{:https://github.com/diskuv/proscript-messaging} clone of that repository} for audit redundancy.

    The [proscript-messaging] directory contains an implementation of X3DH and Double Ratchet in a
    restricted form of Javascript ("ProScript") suited to security analysis. There is also a
    small security library written in ProScript called the
    ProScript Cryptographic Library (PSCL) containing security primitives like
    SHA-256. [proscript-messaging] also contains an {e OCaml}
    parser and an Abstract Syntax Tree (AST) for that restricted Javascript, which it uses to
    translate the X3DH and Double Ratchet implementations
    into a few other languages.

    With [Make]:
    - the ProScript implementations of X3DH and Double Ratchet are used {e without modification}
    - the OCaml parser and OCaml AST are used {e without modification}
    - there is a hand-written translator called [ps2ocaml] from ProScript into OCaml that makes use of the OCaml AST
    - there is this functor that wraps the X3DH and Double Ratchet machine translated OCaml
      into statement-by-statement equivalent OCaml

    Please refer to {{:https://diskuv.github.io/dirsp-exchange/src-proscript/proscript-messaging/KBB2017_FILES.html} File Structure for Auditing KBB2017}
    for complete details.

    {1:ManagingState Managing State}

    The second parameter named [them] in {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.PROTOCOL.TOPLEVEL.send} and
    {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.PROTOCOL.TOPLEVEL.recv} requires special handling.
    You must set [them] to the last [record_them] used in a conversation {b or you lose forward secrecy!}.

    We suggest that you use the next section "Serialization and Deserialization" so you can save the last [record_them] into secure storage,
    and have it available when you need to [send] or [recv].

    Capture the record from the following scenarios:

    + When you create a session with {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.PROTOCOL.TOPLEVEL.newSession}, the return
      value is a [record_them].
    + When you send a message with {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.PROTOCOL.TOPLEVEL.send}, the return value
      has an {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.record_sendoutput.them} field which is
      a [record_them].
    + When you receive a message with {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.PROTOCOL.TOPLEVEL.recv}, the return value
      has an {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.record_recvoutput.them} field which is
      a [record_them].

    {1:Marshalling Serialization and Deserialization}

    The records can all be serialized into and out of protobuf. Here we encode
    [bobFromAliceReceiveOutput2.them]
    from the {!section:Summary} into bytes, and then we decode those bytes back into
    its original type {!Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf.record_them}:

    {[
      module KBBI = Dirsp_exchange_kbb2017__.Kobeissi_bhargavan_blanchet_intf

      let encoder = KBBI.record_them_to_protobuf P.t_to_protobuf
      let decoder = KBBI.record_them_from_protobuf P.t_from_protobuf

      let (encoded_bytes: Bytes.t) =
        Protobuf.Encoder.encode_exn
          encoder
          bobFromAliceReceiveOutput2.them
      (* You can save the [encoded_bytes] in secure storage *)

      let (decoded_value: K.t KBBI.record_them) =
        Protobuf.Decoder.decode_exn
          decoder
          encoded_bytes
      (* Now you have a [record_them] *)
    ]}

    The complete list of parametric records that can be saved is at {!Kobeissi_bhargavan_blanchet_intf}.

    See {{:https://github.com/ocaml-ppx/ppx_deriving_protobuf} ppx_deriving_protobuf} for details about the protobuf encoding.
*)
module Make : Kobeissi_bhargavan_blanchet_intf.PROTOCOLFUNCTOR
