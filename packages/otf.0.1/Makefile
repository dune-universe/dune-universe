.PHONY: all check clean check-config check-simple-commands check-stdio check-collections install uninstall package

all:
	dune build -p otf
	echo "#!/bin/sh" > otf
	echo "dune exec -- ./otf.exe \"\$$@\"" >> otf
	chmod +x otf


check-config: all
	./otf -print-config | diff - tests/default-config
	./otf -print-config tests/one-command/true-command | diff - tests/one-command/true-command.expected-config
	./otf -print-config tests/one-command/false-command | diff - tests/one-command/false-command.expected-config
	./otf -print-config tests/common-config/false-command.test | diff - tests/common-config/false-command.expected-config
	./otf -silent tests/broken-configs/check-invalid-intval.test
	./otf -silent tests/broken-configs/check-invalid-element

check-simple-commands: all
	./otf -silent tests/one-command/true-command
	./otf -silent tests/one-command/false-command
	./otf tests/one-command/wrong-false-command 2>&1 | diff - tests/one-command/wrong-false-command.expected-output
	./otf tests/one-command/wrong-true-command 2>&1 | diff - tests/one-command/wrong-true-command.expected-output

check-stdio: all
	./otf -silent tests/stdin/succesful-grep
	./otf -silent tests/stdin/failing-grep
	./otf -silent tests/stdout-stderr/succesful-stdout-comparison
	./otf -silent tests/stdout-stderr/print-stuff
	./otf tests/stdout-stderr/failed-stdout-comparison 2>&1 | diff - tests/stdout-stderr/failed-stdout-comparison.expected-output
	./otf -max-diff 9 tests/stdout-stderr/failed-stdout-comparison 2>&1 | diff - tests/stdout-stderr/failed-stdout-comparison.expected-truncated-output

check-collections: all
	./otf -silent tests/collections/one-command-collection
	./otf -silent tests/collections/common-config-collection

check: check-config check-simple-commands check-stdio check-collections

install:
	dune install otf

uninstall:
	dune uninstall otf


clean:
	dune clean
	rm -f otf
