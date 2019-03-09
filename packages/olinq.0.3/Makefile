
all: build test

build:
	dune build @install

clean:
	dune clean

test:
	dune runtest --force --no-buffer

doc:
	dune build doc

VERSION ?= dev

update_next_tag:
	@echo "update version to $(VERSION)..."
	zsh -c 'sed -i "s/NEXT_VERSION/$(VERSION)/g" **/*.ml **/*.mli'
	zsh -c 'sed -i "s/NEXT_RELEASE/$(VERSION)/g" **/*.ml **/*.mli'

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make ; \
	done

.PHONY: all clean qtest-gen qtest-clean test update_next_tag doc
