Network Block Device
====================

[![Build Status](https://travis-ci.org/xapi-project/nbd.svg?branch=master)](https://travis-ci.org/xapi-project/nbd)
[![Coverage Status](https://coveralls.io/repos/xapi-project/nbd/badge.svg?branch=master)](https://coveralls.io/r/xapi-project/nbd?branch=master)

A pure OCaml implementation of the [Network Block
Device](http://en.wikipedia.org/wiki/Network_block_device) protocol, which is a
client/server protocol for accessing block devices.

This repository provides the following OPAM packages:

* `nbd` : core protocol parsing library
* `nbd-lwt-unix` : `Lwt_unix` implementation
* `nbd-tool`: command line helper for serving and mirroring disks over NBD, and
  getting information about the disks exported by an NBD server

Each of these OPAM packages installs an ocamlfind library with the same name as
the OPAM package.

Documentation
-------------

The [API documentation is on github](https://xapi-project.github.io/nbd/index.html).
