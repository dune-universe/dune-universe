OPAM_DEPENDS="ounit jbuilder"

ppa=avsm/ocaml42+opam12
echo "yes" | sudo add-apt-repository ppa:$ppa

sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
opam init
opam update
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
make test
