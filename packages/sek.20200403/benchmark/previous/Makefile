SHELL := bash

# The opam switch that we want to use.
# This setting can be overridden on the command line.

SWITCH := 4.09.1+flambda

# The name of this benchmark subdirectory.

THIS   := $(shell basename `pwd`)

# The name of the executable built by dune.

MAIN   := ../../_build/$(SWITCH)/benchmark/$(THIS)/src/Main.exe

# By default, we benchmark only one binary, but it is possible to
# benchmark several distinct binaries by overriding this setting and
# providing a comma-separated list of binaries.

PROGS  := $(MAIN)

##############################################################################
# Compilation.

.PHONY: all binary
all: binary

binary:
	@(echo "(lang dune 2.0)" && \
	  echo "(context (opam (switch $(SWITCH))))" \
	 ) > dune-workspace.local
	@ echo Building using OCaml $(SWITCH)...
	@ dune build --profile release --workspace dune-workspace.local .

##############################################################################
# Creating the required opam switch.
# Run this only once.
# This takes several minutes.

.PHONY: setup
setup:
	@ CURRENT=`opam switch show` ; \
	if opam switch list | grep $(SWITCH) >/dev/null ; then \
	  echo "The switch $(SWITCH) already exists." ; \
	  opam switch $(SWITCH) ; \
	else \
	  echo "Creating switch $(SWITCH)..." ; \
	  opam switch create $(SWITCH) ; \
	fi ; \
	eval $$(opam env) ; \
	echo "Installing required packages..." ; \
	opam install --yes pprint cppo ; \
	echo "Switching back to $$CURRENT..." ; \
	opam switch set $$CURRENT >/dev/null ; \

##############################################################################
# Cleanup.

.PHONY: clean
clean:
	@ dune clean
	@ rm -rf `cat .gitignore`

.PHONY: realclean
realclean: clean
	@ rm -f *.pdf

##############################################################################
# A basic test.

.PHONY: test
test: binary
	$(MAIN) -seq ESek -nb_repeat 1 -n 1000000

##############################################################################
# Benchmarks with plots.

# This assumes that the tools [prun] and [pplot] tools are available.
#   git clone git@github.com:deepsea-inria/pbench.git
#   cd pbench
#   make install

PRUN  = prun
PPLOT = pplot

# The following parameters are common to all benchmarks:

COMMON  = \
  -runs 1 \
  -timeout 5 \

# Typical kinds of graphs.

LENGTH_EXECTIME = \
  scatter --yzero --xlog -x length -y exectime -legend-pos topleft

# A naming convention for the output plots.

OUTPUT  = \
  -output plots_$(SWITCH)_$@.pdf

##############################################################################
# Viewing the most-recently-generated PDF file.

.PHONY: view
view:
	@ FILE=`ls -t -1a *.pdf | head -1` && \
	if [ -x /usr/bin/open ] ; then \
	  open $$FILE ; \
	elif command -v evince >/dev/null ; then \
	  evince $$FILE ; \
	fi

##############################################################################
# Comparing many implementations of sequences.

SEQS := StackListRef,StackFixedArray,StackOCamlStdlib,StackVector,ESek,PSek

.PHONY: bench_fast
bench_fast: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 5000000 \
	  -length 10,1000,10000,100000,500000 \
	  -seq $(SEQS) \
	  -chunk_capacity 128 \
	  -deep_chunk_capacity 16 \
	  $(COMMON)
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -series seq \
	  $(OUTPUT)

.PHONY: bench_full
bench_full: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 40000000 \
	  -length 2,10,100,1000,10000,100000,1000000,10000000,20000000,40000000 \
	  -seq $(SEQS) \
	  -chunk_capacity 128 -deep_chunk_capacity 16 \
	  -runs 3
	  -timeout 10
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -series seq \
	  $(OUTPUT)

##############################################################################
# Comparing OCaml versus OCaml+flambda.

.PHONY: compare
compare:
	for switch in 4.09.1 4.09.1+flambda ; do \
	  make SWITCH=$$switch binary ; \
	  ln -sf ../../_build/$$switch/benchmark/$(THIS)/src/Main.exe ./$$switch ; \
	done
	@ $(PRUN) -prog 4.09.1,4.09.1+flambda -test stack_repeat_pushn_popn \
	  -n 40000000 \
	  -length 2,10,100,1000,10000,100000,1000000,10000000,20000000,40000000 \
	  -seq ESek,PSek \
	  -chunk_capacity 128 -deep_chunk_capacity 16 \
	  -runs 2 -timeout 15
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -series seq,prog \
	  -output ocaml-versus-flambda.pdf

##############################################################################
# Comparing two commits.

# Use [make commits] or [make BASE=<commit> commits].

# The current HEAD is compared against $(BASE), which by default is HEAD^.
# The variable BASE can be overridden on the command line.

BASE = HEAD^

.PHONY: one_commit
one_commit: binary
	@ commit=`git rev-parse --short HEAD` && \
	  ln -sf $(MAIN) $$commit && \
	  $(PRUN) -prog $$commit -test stack_repeat_pushn_popn \
	  -n 4000000 \
	  -length 2,10,100,1000,10000,100000,1000000 \
	  -seq ESek,PSek \
	  -chunk_capacity 128 -deep_chunk_capacity 16 \
	  -runs 3 -timeout 5 \
	  --append \
	  && rm -f $$commit

.PHONY: commits
commits:
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  fi
	@ rm -f results.txt
# Protect against changes to this Makefile that might occur
# as we go back into the past.
	@ cp -f Makefile /tmp/Makefile
# Benchmark the previous commit.
	@ git rev-parse HEAD > current.commit
	@ git co $(BASE)
	@ make -f /tmp/Makefile one_commit
# Benchmark this commit.
	@ git co `cat current.commit` && rm -f current.commit
	@ make -f /tmp/Makefile one_commit
# Plot.
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -series seq,prog \
	  -output commit-versus-commit.pdf

##############################################################################
# Comparing several settings for the size of the outermost chunks.

.PHONY: study_chunk_capacity
study_chunk_capacity: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 500000 \
	  -length 10,1000,100000,500000 \
	  -chunk_capacity 32,64,128,256 \
	  -deep_chunk_capacity 32 \
	  -seq ESek,PSek \
	  $(COMMON)
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -chart seq \
	  -series chunk_capacity \
	  $(OUTPUT)

##############################################################################
# Studying what happens in a specific "worst-case" scenario.

.PHONY: study_worst_case
study_worst_case: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 5000000 \
	  -length 1,10,100,1000,500000 \
	  -nb_init 0,50,256,255,257 \
	  -chunk_capacity 128 -deep_chunk_capacity 32 \
	  -seq ESek \
	  $(COMMON)
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -chart seq \
	  -series nb_init \
	  $(OUTPUT)

##############################################################################
# Studying whether using a functor imposes an overhead.

.PHONY: study_no_functor
study_no_functor: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 16777216 \
	  -length 1,32,1024,131072,4194304,16777216 \
	  -seq StackVector,StackVectorNoFunctor \
	  -runs 3 \
	  -timeout 5
	@ $(PPLOT) $(LENGTH_EXECTIME) \
	  -series seq \
	  $(OUTPUT)

##############################################################################
# Studying the cost of overwriting empty slots with default values

study_overwrite_empty_slots: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 10000000 \
	  -length 2,10,1000,1000000,10000000,10000000 \
	  -seq $(SEQS) \
	  -chunk_capacity 128 \
	  -deep_chunk_capacity 16 \
	  -overwrite_empty_slots 0,1 \
	  -runs 3 \
	  -timeout 5
	@ $(PPLOT) $(LENGTH_EXECTIME) \
     -chart seq \
	  -series overwrite_empty_slots \
	  -legend-pos bottomleft \
	  $(OUTPUT)

################################################################
# EFFECT OF MINOR HEAP SIZE

stack_minor_heap_size: binary
	@ $(PRUN) -prog $(PROGS) -test stack_repeat_pushn_popn \
	  -n 10000000 \
	  -length 2,10,1000,1000000,10000000 \
	  -seq $(SEQS) \
	  -chunk_capacity 128 \
	  -deep_chunk_capacity 16 \
	  -minor_heap_multiple_of_32k 1,4,16,32,128,256 \
	  -runs 2 \
	  -timeout 5
	@ $(PPLOT) $(LENGTH_EXECTIME) \
     -chart seq \
	  -series minor_heap_multiple_of_32k \
	  -legend-pos topleft \
	  $(OUTPUT)
