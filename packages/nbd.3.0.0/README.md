Network Block Device
====================

[![Build Status](https://travis-ci.org/xapi-project/nbd.svg?branch=master)](https://travis-ci.org/xapi-project/nbd)
[![Coverage Status](https://coveralls.io/repos/xapi-project/nbd/badge.svg?branch=master)](https://coveralls.io/r/xapi-project/nbd?branch=master)


A pure OCaml implementation of the [Network Block
Device](http://en.wikipedia.org/wiki/Network_block_device) protocol, which is a
client/server protocol for accessing block devices.

This library installs the following ocamlfind packages:

* `nbd` : core protocol parsing library
* `nbd.lwt` : `Lwt_unix` implementation 

It also installs the `nbd-tool` command line helper.

Documentation
-------------

The [API documentation is on github](https://xapi-project.github.io/nbd/index.html).
