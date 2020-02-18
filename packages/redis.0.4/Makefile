
all: build test

build:
	@dune build @all

test:
	@dune runtest --force --no-buffer

clean:
	@dune clean

watch:
	@dune build @all -w

reindent:
	@for dir in src examples tests/ ; do \
	  find $(dir) -name '*.ml*' -exec ocp-indent -i {} \; ; \
	done

.PHONY: all build test clean watch
