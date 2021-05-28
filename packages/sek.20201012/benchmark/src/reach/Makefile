.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for distance in -1 128 ; do \
	  dune exec --profile release -- ./Main.exe -n 1000000 -distance $$distance -dry-run 1 ; \
	done
