# ocaml-dns-forward
Library and tools for creating forwarding DNS servers

[![Build Status](https://travis-ci.org/djs55/ocaml-dns-forward.png?branch=master)](https://travis-ci.org/djs55/ocaml-dns-forward) [![Coverage Status](https://coveralls.io/repos/djs55/ocaml-dns-forward/badge.png?branch=master)](https://coveralls.io/r/djs55/ocaml-dns-forward?branch=master)

There is no API documention or example code yet.

Features

- UDP and TCP DNS forwarding
- support for sending queries to specific servers based on domain
- dynamic configuration updates
- extra records (e.g. from /etc/hosts)

## Simple usage

```sh
make
./_build/bin/main.native doc/example.config
```
and then send queries as follows:
```
dig @127.0.0.1 -p 5555 www.google.com
dig @127.0.0.1 -p 5555 www.docker.com
```

## References

- [DNS Transport over TCP - Implementation Requirements](https://tools.ietf.org/html/rfc5966)
