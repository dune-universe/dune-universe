# Hacking the build into Travis-CI "C" environment
# See http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

OPAM_PACKAGES='ocamlfind base-bytes'

export OPAMYES=1
opam init
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`
opam install -q -y ${OPAM_PACKAGES}

# compile ocaml-ogg
git clone https://github.com/savonet/ocaml-ogg.git
cd ocaml-ogg && opam pin add . -y && cd ..
# compile & run tests
./bootstrap && ./configure && make && make -C examples test
