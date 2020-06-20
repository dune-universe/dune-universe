.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for seq in ESek PSek ; do \
	  for op in push init make ; do \
	    dune exec --profile release -- ./Main.exe -n 1000 -seq $$seq -op $$op -dry-run 1 ; \
	  done ; \
	done
	for seq in Array ; do \
	  for op in init make ; do \
	    dune exec --profile release -- ./Main.exe -n 1000 -seq $$seq -op $$op -dry-run 1 ; \
	  done ; \
	done
