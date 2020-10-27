Waylaunch is a program launcher for Wayland.

Only compositors that implement the [wlr-layer-shell](https://github.com/swaywm/wlr-protocols/tree/master/unstable) protocol are supported at the moment.
Typically [wlroots](https://github.com/swaywm/wlroots)-based compositors.

Waylaunch includes a command history by default.

## Requirements

OCaml requirements (installable via [opam](https://github.com/ocaml/opam)):
  - `ocaml` >= 4.11
  - `dune` >= 2.7
  - `dune-configurator` >= 2.7
  - `bos` >= 0.2.0
  - `fpath` >= 0.7.0
  - `rresult` >= 0.6.0

C requirements:
  - `pkg-config`
  - `libwayland`
  - `wayland-protocols`
  - `cairo`
  - `pango`
  - `xkbcommon`

You can install all the requirements easily and in a way that is cross-platform using opam:
```
$ git submodule update --init
$ opam pin add -n .
$ opam depext waylaunch
$ opam install --deps-only waylaunch
```

## Installation

To install it:
```
$ dune build -p waylaunch
$ sudo $(which dune) install --prefix /usr/local --sections bin
```

## Technologies

This program is written in OCaml and is currently using a fork of [bemenu](https://github.com/kit-ty-kate/bemenu) (written in C) underneath. This fork of bemenu has to vocation to disappear little by little in the future as the code will ultimately be rewritten in OCaml.

## License

Due to the distribution of the client code from bemenu, this whole project is licensed under GNU GPLv3 (and GNU LGPLv3 for bemenu's own library and bindings)
