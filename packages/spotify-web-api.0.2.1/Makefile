all: build

ATDGEN_FILES=$(wildcard lib/*.atd)
ATDGEN_J_FILES=$(ATDGEN_FILES:.atd=_j.ml)
ATDGEN_T_FILES=$(ATDGEN_FILES:.atd=_t.ml)

lib/%_j.ml: lib/%.atd
	atdgen -j $<

lib/%_t.ml: lib/%.atd
	atdgen -t $<

build: $(ATDGEN_J_FILES) $(ATDGEN_T_FILES)
	dune build @install

test: build
	rm -rf _build/default/test/data
	mkdir -p _build/default/test/data
	cp -r test/data/* _build/default/test/data/
	dune runtest --force

install: build
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	$(foreach FILE, $(ATDGEN_FILES), rm -f `atdgen -list $(FILE)`)
