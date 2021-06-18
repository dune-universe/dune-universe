# OCaml-redis [![Build](https://github.com/0xffea/ocaml-redis/workflows/Build%20redis/badge.svg)](https://github.com/0xffea/ocaml-redis/actions)

Client library for [Redis](http://redis.io/) in pure OCaml.

Changelog can be found in [CHANGES.md](/CHANGES.md) file.

## Dependencies

- For `redis-lwt`, [Lwt](http://ocsigen.org/lwt/install) is needed.

## Documentation

http://0xffea.github.io/ocaml-redis/

## Quick start

### Installation

`ocaml-redis` implements synchronous and lwt clients. Each of them is in a separate package.

- synchronous version:
  ```
  opam install redis-sync
  ```
- lwt version:
  ```
  opam install redis-lwt
  ```

**Note**: connections are not safe to share among threads.

## Contribution

The tests require [Docker](https://docs.docker.com/get-docker/) and [docker-compose](https://docs.docker.com/compose/install/).

Once they have been installed, the tests can be run with `make test`.
