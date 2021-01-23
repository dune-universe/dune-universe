PROFILE=release

.PHONY: build clean test all

build:
	dune build @install --profile=$(PROFILE)

test:
	dune build --profile=$(PROFILE) test/client_test.exe
	dune build --profile=$(PROFILE) test/server_test.exe
	
all:
	dune build @all --profile=$(PROFILE)

install:
	dune install --profile=$(PROFILE)

uninstall:
	dune uninstall --profile=$(PROFILE)

clean:
	rm -rf _build
