.PHONY: all clean utop test doc show-deps install uninstall

DUNE = dune

TARGET = electrod

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

all: build

build:
	dune build @install \
	&& ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

watch:
	dune build --watch @install

test-release:
	dune build -p electrod --workspace dune-workspace.release @runtest @install 

release: 
	dune subst 
	dune build -p electrod @install
	cp _build/install/default/bin/$(TARGET) $(RELEASE)
	strip $(RELEASE)

fmt:
	dune build @fmt --auto-promote

test:
	dune runtest 

utop:
	dune utop --profile release

doc:
	dune build @doc && x-www-browser _build/default/_doc/_html/index.html

show-deps:
	dune external-lib-deps --missing @install

install: build
	@dune install

uninstall:
	@dune uninstall

clean:
	@dune clean
	@git clean -dfXq
	@rm -f ./$(TARGET) electrod.opam electrod.install

include $(shell ocamlfind query visitors)/Makefile.preprocess
