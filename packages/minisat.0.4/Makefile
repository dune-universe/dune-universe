
DUNE_OPTS=--profile=release 

build:
	@dune build $(DUNE_OPTS)

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --force --no-buffer

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	@dune build @install  $(DUNE_OPTS) -w
