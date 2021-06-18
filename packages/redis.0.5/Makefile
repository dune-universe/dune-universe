
all: build test

build:
	@dune build @all

test:
	@docker-compose up -d
	@(dune runtest --force --no-buffer; EXIT_CODE="$$?"; docker-compose down; exit $$EXIT_CODE)

clean:
	@dune clean

watch:
	@dune build @all -w

reindent:
	@for dir in src examples tests/ ; do \
	  find $(dir) -name '*.ml*' -exec ocp-indent -i {} \; ; \
	done

.PHONY: all build test clean watch
