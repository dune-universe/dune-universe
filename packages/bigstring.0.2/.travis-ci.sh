# Inspired from repo's .travis-ci.sh ocaml/oasis2opam
export OPAMYES=1

if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi

if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install ocamlfind jbuilder

export OCAMLRUNPARAM=b

make
make doc

opam pin add bigstring .
opam remove bigstring
[ -z "`ocamlfind query bigstring`" ] || (echo "It uninstalled fine!" && exit 1)
