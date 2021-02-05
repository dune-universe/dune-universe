
default: build

.PHONY: build syslog.install install uninstall doc clean

build:
	dune build

syslog.install:
	dune build @install

install: syslog.install
	dune install syslog

uninstall: syslog.install
	dune uninstall syslog

doc:
	dune build @doc

clean:
	dune clean
