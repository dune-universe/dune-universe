# Hacking the build into Travis-CI "C" environment
# See http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

export OPAMYES=1
opam init
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`
opam install -y depext dune
opam pin -y add --no-action .
opam depext -y -i duppy

# compile
dune build
