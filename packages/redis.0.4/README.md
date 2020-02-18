[![Build Status](https://travis-ci.org/0xffea/ocaml-redis.svg?branch=master)](https://travis-ci.org/0xffea/ocaml-redis)

# Ocaml-redis

Ocaml bindings for [Redis](http://redis.io/).

Changelog can be found in [CHANGES.md](/CHANGES.md) file.

## Dependencies

- For `redis-lwt`, [Lwt](http://ocsigen.org/lwt/install) is needed.

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

## Documentation

http://0xffea.github.io/ocaml-redis/
