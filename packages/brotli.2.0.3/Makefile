.PHONY: clean

build:
	@jbuilder build -p brotli

install:build
	jbuilder install

remove:; jbuilder uninstall

docs:build
	jbuilder build @doc
	mv _build/default/_doc docs

clean:; jbuilder clean
