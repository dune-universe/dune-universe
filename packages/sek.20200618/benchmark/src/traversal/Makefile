.PHONY: all clean
all clean:
	make -C .. $@

.PHONY: test
test:
	for traversal in baseline bfs/ESek bfs/Queue dfs/ESek dfs/Stack dfs/StackFixedArray ; do \
	  dune exec --profile release -- ./Main.exe -dry-run 1 \
	        -n 1000000 -d 5 -traversal $$traversal ; \
	done
