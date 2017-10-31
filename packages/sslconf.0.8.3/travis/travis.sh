#!/usr/bin/env bash

# ---------------------------------------------------------------------------
#  Copyright (c) 2017 Tony Wuersch. All rights reserved.
#  Distributed under the ISC license, see terms at the end of the file.
#  sslconf 0.8.3 - commit 0806f5ae3774c30cc8ee18aaabe9d106d7816457
# ---------------------------------------------------------------------------

set -o errexit -o nounset
set -x

travis_install_on_linux () {
    # Install OCaml and OPAM PPA
    sudo add-apt-repository -y ppa:avsm/ocaml42+opam12
    sudo apt-get update -qq

    sudo apt-get install -qq opam time git

    case "$OCAML_VERSION" in
        4.03)
            opam init -y --compiler=4.03.0 ;;
        4.04)
            opam init -y --compiler=4.04.2 ;;
        4.05)
            opam init -y --compiler=4.05.0 ;;
        4.06)
            opam init -y --compiler=4.06.0+trunk ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

travis_install_on_osx () {
    brew update > /dev/null
    brew install opam

    case "$OCAML_VERSION" in
        4.03)
            opam init -y --compiler=4.03.0 ;;
        4.04)
            opam init -y --compiler=4.04.2 ;;
        4.05)
            opam init -y --compiler=4.05.0 ;;
        4.06)
            opam init -y --compiler=4.06.0+trunk ;;
        *)
            echo Unknown $OCAML_VERSION
            exit 1 ;;
    esac
}

case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# Prepare environment
eval $(opam config env)

# Check packages
ocaml -version | grep $OCAML_VERSION
opam --version
git --version

opam update
opam install -y astring ppx_sexp_conv fpath cmdliner rresult oUnit
# opam depext -uivyj 2 sslconf

cd lib

echo
echo "Building"
echo
make build

echo
echo "Testing"
echo
make runtest

cd ..

# ---------------------------------------------------------------------------
#  Copyright (c) 2017 Tony Wuersch

#  Permission to use, copy, modify, and/or distribute this software for any
#  purpose with or without fee is hereby granted, provided that the above
#  copyright notice and this permission notice appear in all copies.

#  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
#  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
#  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
#  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
#  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
#  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
#  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
# ---------------------------------------------------------------------------
