.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for op in fill blit ; do \
	  for construction in push assemble share ; do \
	    dune exec --profile release -- ./Main.exe -n 100000 -construction $$construction -op $$op -dry-run 1 ; \
	  done ; \
	done
