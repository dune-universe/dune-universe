# dirsp-exchange

"dirsp" is a short form for Diskuv Implementations of Research Security Protocols.

The first protocol we've included is "KBB2017" which is what the authors (security researchers) call a variant of
[Signal Protocol v3](https://www.signal.org/docs/specifications/x3dh/).
We love it because it has been formally verified using two complementary proof checking tools. With the programming language OCaml
and `dirsp-exchange` you could write secure code like:

```ocaml
module P       = Dirsp_proscript_mirage.Make()
module ED25519 = P.Crypto.ED25519
module K       = Dirsp_exchange_kbb2017.Make(P)
module U       = K.UTIL

(* Alice sends a message to Bob *)
let aliceSessionWithBob = T.newSession (* ... supply some keys you create with ED25519 and U ... *) ;;
let aliceToBobSendOutput = T.send
  aliceIdentityKey
  aliceSessionWithBob
  (P.of_string "Hi Bob!")

(* Now you can send the output "aliceToBobSendOutput" from Alice to Bob.
   Let's switch to Bob's computer. He gets notified of a new message using a notification library of your choosing, and then does ...  *)

let bobSessionWithAlice = T.newSession (* ... supply some keys ... *);;
let bobFromAliceReceiveOutput = T.recv
  bobIdentityKey
  bobSignedPreKey
  bobSessionWithAlice
  theEncryptedMessageBobReceivedFromAlice
assert (bobFromAliceReceiveOutput.output.valid)
Format.printf "Bob just received a new message: %s\n"
  (bobFromAliceReceiveOutput.plaintext |> P.to_bytes |> Bytes.to_string)
```

Bindings to other languages and implementations of other security algorithms may follow
later.

> Looking for other implementations? Skip down to [Comparison to Other Libraries](#comparison_to_other_libraries)

The intent of the **[dirsp]** libraries is to provide software engineers with auditable
source code that has some level of safety assurance (typically proofs) from security researchers.
By "auditable" we mean the ability to justify every line of source code when undergoing an audit
by a competent security engineer. No third-party vetting of the source code has been
conducted (unless noted explicitly), and the original authors at Diskuv did not have security
researchers or engineers on staff when the libraries were originally written.
Contact [security@diskuv.com](mailto:security@diskuv.com) to report any security issues, and feel
free to publicly shame the [Twitter handle @diskuv](https://twitter.com/diskuv) if Diskuv is not being
responsive.

The implementations in this library are licensed permissively to broaden use and scrutiny. Sometimes
that means writing an implementation from scratch based only on an academic paper.
In contrast, placing security primitives like KBB2017 under restrictive licenses (ex. GPL and especially AGPL)
discourages scrutiny because many security engineers work for
companies which discourage or [prohibit](https://opensource.google/docs/using/agpl-policy/)
restrictive licenses. Note this lessened scrutiny is
particular to _low level security libraries_ that are restrictively licensed; even the original author of these libraries
will use copy-left licenses for other types of libraries and applications.

## Programming Languages

TLDR: We don't expect you to run your code in OCaml. Embed it in your host language instead.

Most of the **[dirsp]** libraries are based in the programming language OCaml. Among other things OCaml is commonly used to
write domain-specific languages for proof analysis. [Coq](https://coq.inria.fr/about-coq) is one example of a
well-known DSL for developing proofs.

ProScript is another DSL. It is a restricted JavaScript language meant to be easily accessible to software engineers,
executed in production yet formally verifiable. The design fits well with the software engineering intent
of **[dirsp]**. The only soft spot is that the ProScript execution model is JavaScript which (at least for the authors of
**[dirsp]**) is not an ideal production language! So we built a tool `dirsp-ps2ocaml` to translate algorithms written
in ProScript into OCaml.

Let's be clear ... we don't think OCaml is an ideal production language for many people either; we suspect many teams will
find the OCaml ecosystem to be too small. But we think there is a navigable path compared to JavaScript for OCaml to be embedded
in several host languages (Java, Objective-C, JavaScript, etc.) and able to run logic (security and privacy algorithms) on many
different host platforms (desktop, server, mobile, etc.).

## Libraries

This repository contains:

- [dirsp-exchange-kbb2017](https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-exchange-kbb2017/) - The KBB2017 protocol for securing a two-party conversation. Similar to
  Signal Protocol v3 [[1]](https://signal.org/docs/specifications/x3dh) [[2]](https://signal.org/docs/specifications/doubleratchet)
  and Olm [[3]](https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md)
- [dirsp-proscript](https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-proscript/) - The ProScript Cryptographic Library (PSCL) API interface
- [dirsp-proscript-mirage](https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-proscript-mirage/) - A PSCL implementation based on `mirage-crypto` and related libraries
- [dirsp-ps2ocaml](https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-ps2ocaml/) - A ProScript to OCaml translator with [documentation available](https://diskuv.github.io/dirsp-exchange/src-proscript/proscript-messaging/PS2OCAML.html).

The online documentation is at:

- [Sphinx Documentation](https://diskuv.github.io/dirsp-exchange)
- [OCaml Libraries](https://diskuv.github.io/dirsp-exchange/ocaml)

## Comparison to Other Libraries

| Library                                                                                                                                                                | Lineage                                              | Language. Bindings                                     | License                           | Papers                                                                                                                                                                          | Usage                                                                                                                        |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- | ------------------------------------------------------ | --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| [dirsp-exchange-kbb2017](https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-exchange-kbb2017/)                                                                        | [Cryptocat](https://en.wikipedia.org/wiki/Cryptocat) | OCaml                                                  | Apache v2                         | [Automated Verification for Secure Messaging Protocols and Their Implementations: A Symbolic and Computational Approach](https://ieeexplore.ieee.org/abstract/document/7961995) | Original code formally verified. **[dirsp]** has not audited                                                                 |
| [libsignal-client (modern)](https://github.com/signalapp/libsignal-client), [libsignal-protocol-java (inactive)](https://github.com/signalapp/libsignal-protocol-java) | [Signal](https://www.signal.org/download/)           | Rust. Java, Swift, and TypeScript                      | AGPLv3 (modern). GPLv3 (inactive) | [Signal Specifications](https://signal.org/docs/)                                                                                                                               | Well-known cryptographers. ["Use outside of Signal is unsupported."](https://github.com/signalapp/libsignal-client#overview) |
| [libolm](https://gitlab.matrix.org/matrix-org/olm)                                                                                                                     | [Matrix](https://matrix.org/)                        | C++ 11. JavaScript, Python, Android Java, Objective-C. | Apache v2                         | [Olm: A Cryptographic Ratchet](https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md)                                                                              | Audited in 2016. [Actively being re-analysed](https://matrix.org/blog/2021/06/14/adventures-in-fuzzing-libolm)               |

Our recommendation for secure 1-on-1 messaging?

- Prefer **[libolm]** over **[dirsp-exchange-kbb2017]** unless you need to go beyond what **[libolm]** offers.
  In other words, use **[dirsp-exchange-kbb2017]** if you need to extend an algorithm in ProScript _or_ need to adopt new published research (please contribute it here if you do!)
- Don't use **[libsignal-client]** because its owners actively discourage you from using it today. But since
  we suspect their stance is based on a lack of engineering resources rather than their [donation-based mission of developing open source privacy technology](https://www.signal.org/donate/), you may want to directly ask them.

## Contributing

Engineers: We are actively looking for a) bindings to other languages and b) implementations of other security algorithms and c) multiple pairs of eyes vetting the code base. Please contribute if any of those interest you,
or if you have other ideas!

Researchers: Looking for a test subject for an upcoming paper? If you use **[dirsp]** you have a reasonable opportunity for your research to impact production systems and apps.

See [Contributing](https://diskuv.github.io/dirsp-exchange/CONTRIBUTING.html)
