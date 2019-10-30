.PHONY : build
build :
	dune build

.PHONY: clean
clean :
	dune clean

.PHONY : docs
docs :
	mkdir -p docs
	dune build @doc
	cp -R _build/default/_doc/_html/* docs
