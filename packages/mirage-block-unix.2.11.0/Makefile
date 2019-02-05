
.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

.PHONY: vm-test
vm-test:
	echo "The VM test requires the linuxkit tool to be installed."
	docker build -t stress -f Dockerfile.stress  .
	linuxkit build stress.yml
	echo "If the VM exits, then the test was successful. Otherwise the logs are in /var/log/stress*"
	linuxkit run -disk `pwd`/stress.img,size=16M stress

