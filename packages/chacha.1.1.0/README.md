[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://abeaumont.github.io/ocaml-chacha)
[![Build Status](https://travis-ci.org/abeaumont/ocaml-chacha.svg?branch=master)](https://travis-ci.org/abeaumont/ocaml-chacha)

# ChaCha family of encryption functions, in OCaml

An OCaml implementation of [ChaCha](https://cr.yp.to/chacha/chacha-20080120.pdf) functions,
both ChaCha20 and the reduced ChaCha8 and ChaCha12 functions.
The hot loop is implemented in C for efficiency reasons.

## Installation

```
opam install chacha
```

## Usage

```ocaml
utop[0]> #require "mirage-crypto";;
utop[1]> #require "mirage-crypto-rng.unix";;
utop[2]> Mirage_crypto_rng_unix.initialize ();;
- : unit = ()
utop[3]> let key = Mirage_crypto_rng.generate 32;;
val key : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 32}
utop[4]> let nonce = Cstruct.create 8;;
val nonce : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 8}
utop[5]> #require "chacha";;
utop[6]> let state = Chacha.create key nonce;;
val state : Chacha.t = <abstr>
utop[7]> Chacha.encrypt (Cstruct.of_string "My secret text") state |> Cstruct.to_string;;
- : string = "\026m.\\363\\026\\263\\207Xg\\256l\\262\\232F"
```

* Key can either 32 (recommended) or 16 bytes
* Chacha state may use a different hashing function,
  the recommended `Chacha_core.chacha20` is used by default.
