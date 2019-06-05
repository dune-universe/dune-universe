nocoiner
========

<div align="center">
  <img
    src="https://marcoonroad.dev/nocoiner/images/nocoiner.png"
    width="200"
    height="200"
    alt="nocoiner logo"/>
</div>

<div align="center">
  <p>
    <span>A Commitment Scheme library for Coin Flipping/Tossing algorithms and sort.</span>
  </p>
</div>

<div align="center">
<!-- Travis CI badge -->
<a
  style="margin: 0.1em;"
  href="https://travis-ci.com/marcoonroad/nocoiner"
  title="Verify the build logs here."><img
  src="https://img.shields.io/travis/com/marcoonroad/nocoiner.svg?logo=travis&style=flat-square"/></a>
<!-- Coveralls badge -->
<a
  style="margin: 0.1em;"
  href="https://coveralls.io/github/marcoonroad/nocoiner"
  title="Verify the detailed test coverage here."><img
  src="https://img.shields.io/coveralls/github/marcoonroad/nocoiner.svg?style=flat-square"/></a>
<!-- License badge -->
<a
  style="margin: 0.1em;"
  href="https://github.com/marcoonroad/nocoiner/blob/master/LICENSE"
  title="Verify the project license here."><img
  src="https://img.shields.io/github/license/marcoonroad/nocoiner.svg?style=flat-square&logo=github"/></a>
<!-- Open PR badge -->
<a
  style="margin: 0.1em;"
  href="https://github.com/marcoonroad/nocoiner/compare"
  title="Interested in contribute with this project?"><img
  src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square&logo=github"/></a>
<!-- Donate BTC badge -->
<a
  style="margin: 0.1em;"
  href="https://www.blockchain.com/btc/address/18h417CACY3ksS1XPLWej7TE6hR9Eb6k34"
  title="Want to give a little donation for author?"><img
  src="https://img.shields.io/badge/donate-BTC-yellow.svg?logo=bitcoin&style=flat-square"/></a>
</div>

<div align="center">
<!-- Docker layers badge -->
<a
  style="margin: 0.1em;"
  href="https://microbadger.com/images/marcoonroad/nocoiner"
  title="Verify the Docker image details here."><img
  src="https://img.shields.io/microbadger/layers/marcoonroad/nocoiner/latest.svg?style=flat-square&logo=docker"/></a>
<!-- Docker size badge -->
<a
  style="margin: 0.1em;"
  href="https://microbadger.com/images/marcoonroad/nocoiner"
  title="Verify the Docker image details here."><img
  src="https://img.shields.io/microbadger/image-size/marcoonroad/nocoiner.svg?style=flat-square&logo=docker"/></a>
<!-- Docker pulls badge -->
<a
  style="margin: 0.1em;"
  href="https://hub.docker.com/r/marcoonroad/nocoiner"
  title="Check the Docker project repository here."><img
  src="https://img.shields.io/docker/pulls/marcoonroad/nocoiner.svg?style=flat-square&logo=docker"/></a>
<!-- Docker stars badge -->
<a
  style="margin: 0.1em;"
  href="https://hub.docker.com/r/marcoonroad/nocoiner"
  title="Check the Docker project repository here."><img
  src="https://img.shields.io/docker/stars/marcoonroad/nocoiner.svg?style=flat-square&logo=docker"/></a>
</div>


### About

This project implements [Commitment Schemes][1] using the
[Galois/Counter Mode][2] (GCM) of secret-key encryption. Because this AES
encryption mode provides both Message _Confidentiality_ and _Integrity_, it fits
perfectly the _Hiding_ and _Binding_ properties of Commitment Schemes.
Confidentiality protects the message against _passive attacks_ while integrity
protects it from _active attacks_. GCM, so, works as an
[Authenticated Encryption][6] where it roughly works as an encryption algorithm
with MAC signatures on cipher data.

The hiding property states that it is impossible to discover the secret with the
commitment data left alone, that is, the commitment receiver can't know the
secret until the commitment sender reveals that through her opening key.

The binding property, on the other hand, ensures invariants on the commitment
sender side. It disallows the sender to change the secret by using a different
opening key. While the sender can refuse to reveal her secret, she can't cheat
on the game. There's a variant of commitment schemes called _Timed Commitments_
where the receiver can brute-force the commitment in the case of the sender
aborting the game by refusing to send the opening key, tho. Another variant
called _Fuzzy Commitments_ accepts some noise during opening phase.

Commitment Schemes are one of the many [Secure Multiparty Computation][3]
protocols/primitives, [Secret Sharing][4] is other famous cryptographic
primitive in such field.


### Installation

For the stable release, just type:

```shell
$ opam install nocoiner
```

To install/test the unstable version on this repository (assuming you're
inside the project's root directory):

```shell
$ make install # 'make uninstall' reverts the changes
```


### Testing

```shell
$ make test
```


### Usage

As library (assuming you have linked the package `nocoiner` below):

```ocaml
let secret = "I have nothing to hide."
let (c, o) = Nocoiner.commit secret

assert (secret = Nocoiner.reveal ~commitment:c ~opening:o)
```

Here, the `Nocoiner.commit` operation is non-deterministic and the
`Nocoiner.reveal` is deterministic. The `Nocoiner.reveal` operation may throw
the following exceptions:
- `Nocoiner.Reasons.InvalidCommitment`, if the parsing of commitment fails.
- `Nocoiner.Reasons.InvalidOpening`, if the opening key contains invalid data.
- `Nocoiner.Reasons.BindingFailure`, if both commitment & opening are unrelated.

As the command-line interface (ignore all the `$` below while typing):

```shell
$ echo "Something not really secret..." > secret.txt
$ cat secret.txt | nocoiner commit \
  --commitment-file=commitment-box.txt \
  --opening-file=opening-key.txt
$ nocoiner reveal \
  --commitment-file=commitment-box.txt \
  --opening-file=opening-key.txt > secret-output.txt
$ cat secret-output.txt
```

The complete API reference is available [here][7]. Coverage reports are
generated too, please refer to the respective [page][8].


### Disclaimer

This library was not fully tested against side-channel attacks. Besides the
good source of entropy by the `nocrypto`'s implementation of Fortuna PRNG
algorithm, AES-GCM mode doesn't work well with huge amount of data. Keep in mind
that the use cases of this library is for Secure Multiparty games such as online
Gambling and Auctions. With other use cases, the security of this cryptographic
primitive can be deemed as flawed.

Note that players can abort in the middle of a Commit-and-Reveal game, so you
should as well deal with that on your code logic. The random encryption key
and input vector only ensure the _uniqueness locally_, it's also possible to
happen collisions of both random data on a distributed setting (it's due the
sources of entropy being remote and different - so commitments and openings
would be identical, think on that even if this probability is small). ~~In such
case, you can either take a fingerprint of the host machine and a timestamp
nonce into account, in the same sense of [Elliott's CUID][5] library~~ (we already
cover that issue of distributed collisions by using a fingerprint of hashed
process context).

  [1]: https://en.wikipedia.org/wiki/Commitment_scheme
  [2]: https://en.wikipedia.org/wiki/Galois/Counter_Mode
  [3]: https://en.wikipedia.org/wiki/Secure_multiparty_computation
  [4]: https://en.wikipedia.org/wiki/Secret_sharing
  [5]: https://github.com/ericelliott/cuid
  [6]: https://en.wikipedia.org/wiki/Authenticated_encryption
  [7]: https://marcoonroad.dev/nocoiner/apiref/nocoiner/Nocoiner/index.html
  [8]: https://marcoonroad.dev/nocoiner/apicov/index.html
