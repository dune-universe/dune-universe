DUNE=dune
FLAGS=
PROJECT_NAME=clangml-transforms

.PHONY : all
all :
	$(DUNE) build $(FLAGS)

.PHONY : clean
clean :
	$(DUNE) clean $(FLAGS)

.PHONY : install
install :
	$(DUNE) build @install $(FLAGS)
	$(DUNE) install $(FLAGS)

.PHONY : doc
doc :
	$(DUNE) build @doc $(FLAGS)

.PHONY : test
test :
	$(DUNE) runtest $(FLAGS)

$(PROJECT_NAME).opam : dune-project $(PROJECT_NAME).opam.template
	dune build $(PROJECT_NAME).opam
