.PHONY: all clean utop test doc show-deps install uninstall regression

DUNE = dune

TARGET = electrod

os := $(shell opam var os)
arch := $(shell opam var arch)

RELEASE = ./$(TARGET).${os}.${arch}

all: build

build:
	$(DUNE) build src/$(TARGET).exe

watch:
	$(DUNE) build --watch @check @fmt --auto-promote --diff-command=-

build-release:
	$(DUNE) build --workspace dune-workspace.release @all

# generate opam file (in particular)
opam:
	$(DUNE) build $(TARGET).opam

fmt:
	@$(DUNE) build @fmt --auto-promote --diff-command=- || true

regression:
	$(DUNE) build @regression

utop:
	$(DUNE) utop --profile release

doc:
	$(DUNE) build @doc && x-www-browser _build/default/_doc/_html/index.html

show-deps:
	$(DUNE) external-lib-deps --missing @install

clean:
	@$(DUNE) clean
	@git clean -dfXq
	@rm -f ./$(TARGET) electrod.install

include $(shell ocamlfind query visitors)/Makefile.preprocess
