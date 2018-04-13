build:
	time -p jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test:
	jbuilder build @runtest

all: build test doc

.PHONY: build install uninstall clean doc test all
