PROFILE=release
.PHONY: build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install --profile=$(PROFILE)

uninstall:
	dune uninstall --profile=$(PROFILE)

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	ocp-indent --inplace **/*.ml*
