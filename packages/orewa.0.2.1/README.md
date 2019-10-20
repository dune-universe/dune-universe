# Orewa

[![Build Status](https://travis-ci.org/Leonidas-from-XIV/orewa.svg?branch=master)](https://travis-ci.org/Leonidas-from-XIV/orewa)

俺は - an Async friendly Redis binding in pure OCaml.

## Installation

It is availaible on OPAM, just

```sh
opam install orewa
```

and off you go! No need to install any native libraries or other dependencies.

## Usage

To connect to a Redis server use `Orewa.connect` which will return a handle to
a connection. From here you can use all the implemented Redis commands in
`Orewa.*` and also close the connection with `Orewa.close`. Or just use
`Orewa.with_connection` to handle connection handling for you.

Orewa opts in to offering an OCaml-centric API, so the commands map to
functions and these take typed arguments. So instead of distinguishing between
seconds and millisecond variants, in Orewa both are represented by a time span
type. Different options to commands are represented by ADTs, to avoid
constructing invalid commands. The results are mapped into the most fitting
OCaml data types. Orewa also avoids exceptions, so the signature will tell you
exactly which error types are expected.

### Examples

The integration test in `src/integration/test.ml` attempts to test all
implemented commands, so it demonstrates how each of them can be used.

## Documentation

The [API documentation](https://leonidas-from-xiv.github.io/orewa/) of the
current release is available online. The naming of the functions follows the
naming of [Redis commands](https://redis.io/commands) so it is simple to adapt
from the Redis docs to OCaml.

## Roadmap

Currently all commands from the string category in Redis 5 are implemented.
Over time this set is meant to be expanded. If you need support for a specific
command, submit a pull request.

For a very preliminary draft of what could be coming over time here's a list of
ideas:

1. Most (all?) sensible Redis commands
1. Transactions in Redis
1. Cluster support, maybe. Unlikely to happen any time soon.
