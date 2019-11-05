# owork

An OCaml implementation of a productivity timer. There is a server component at the moment which accepts connections over a Unix socket. The timer can then be started, paused/resumed, reset and sessions restarted.

## Install

To install the program, first clone the repo and then run `make install` (Note: `opam` may be required to install necessary packages):
```sh
git clone https://github.com/jeffa5/owork.git
cd owork
make install
owork --help
```

## Socket file

The default socket file is created at `/tmp/owork.sock`. This can be overridden with a command-line option.

## Protocol

The protocol is outlined in [PROTOCOL.md](PROTOCOL.md)

## Examples

Some example client scripts to interact with the server can be found in the `examples` directory.
