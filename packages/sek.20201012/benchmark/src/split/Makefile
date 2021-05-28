.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for seq in PSek ESek ; do \
	  for op in get drop take split concat create create+reach reach ; do \
	    for i in 1000 ; do \
	      dune exec --profile release -- ./Main.exe -n 100000 -i $$i -op $$op -seq $$seq -dry-run 1 ; \
	    done ; \
	  done ; \
	done
