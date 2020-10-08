all: build

build:
	@dune build

install:
	@dune install

test: build
	@dune runtest

doc: build
	@opam install odoc
	@dune build @doc

clean:
	@dune clean

# Create a release on Github, then run git pull
publish:
	@git tag 0.11
	@git push origin 0.11
	@git pull
	@opam pin .
	@opam publish https://github.com/chrisnevers/mysql8/archive/0.11.tar.gz
