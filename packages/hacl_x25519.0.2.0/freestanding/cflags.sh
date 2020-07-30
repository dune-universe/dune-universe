#!/bin/sh
export PKG_CONFIG_PATH="$(opam config var lib)/pkgconfig"
flags="$(pkg-config --static ocaml-freestanding --cflags)"
echo "(-I . -I kremlin/include -I kremlin/kremlib/dist/minimal $flags)"
