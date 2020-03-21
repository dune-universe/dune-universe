sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install ocaml-migrate-parsetree jbuilder
jbuilder build