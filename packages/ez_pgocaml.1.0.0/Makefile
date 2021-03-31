

all: build

build:
	dune build

install:
	dune install

clean:
	dune clean

doc:
	dune build @doc
	@mkdir -p docs
	@cp -rf _build/default/_doc/_html/* docs
