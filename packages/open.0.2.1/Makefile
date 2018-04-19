build:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

doc:
	jbuilder build @doc

pushdoc: doc
	git checkout gh-pages
	cp -r _build/default/_doc/* .
	git commit -a
	git push
	git checkout master

test:
	jbuilder build @runtest

all: build test doc

.PHONY: build install uninstall clean doc test all pushdoc
