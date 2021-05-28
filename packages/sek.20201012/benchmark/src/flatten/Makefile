.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for seq in ESek PSek ; do \
	  dune exec --profile release -- ./Main.exe -m 10 -n 1000 -seq $$seq -dry-run 1 ; \
	done
