.PHONY: build clean edit install uninstall reinstall

build:
	dune build @install -j 16

clean:
	rm -rf _build linwrap_train_*.txt.model

edit:
	emacs src/*.ml TODO commands.sh &

install: build
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install
