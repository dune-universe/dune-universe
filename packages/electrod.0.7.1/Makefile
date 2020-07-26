.PHONY: all clean utop test doc show-deps install uninstall regression

TARGET = electrod

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

all: build

build:
	dune build src/$(TARGET).exe

watch:
	dune build --watch @check @fmt --auto-promote --diff-command=-

build-release:
	dune build --workspace dune-workspace.release @all

# generate opam file (in particular)
opam:
	dune build $(TARGET).opam

fmt:
	@dune build @fmt --auto-promote --diff-command=- || true

regression:
	dune build @regression

utop:
	dune utop --profile release

doc:
	dune build @doc && x-www-browser _build/default/_doc/_html/index.html

show-deps:
	dune external-lib-deps --missing @install

clean:
	@dune clean
	@git clean -dfXq
	@rm -f ./$(TARGET) electrod.install

include $(shell ocamlfind query visitors)/Makefile.preprocess
