.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for construction in push assemble share ; do \
	  for seq in PSek ESek ; do \
	    methods="get iter iters gam gsaj" ; \
	    if [ "$$seq" == "ESek" ] ; then methods="$$methods sam gsam" ; fi ; \
	    for method in $$methods ; do \
	      dune exec --profile release -- ./Main.exe -dry-run 1 \
	        -n 1000000 -method $$method -construction $$construction -seq $$seq ; \
	    done ; \
	  done ; \
	done
