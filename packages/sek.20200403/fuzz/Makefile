SHELL := bash

# ----------------------------------------------------------------------------

# [make all] compiles the fuzzing code in an appropriate opam switch.

SWITCH := 4.09.0+afl

.PHONY: all
all:
	@ dune build @check # build src/.merlin, etc.
	@(echo "(lang dune 2.0)" && \
	  echo "(context (opam (switch $(SWITCH))))" \
	 ) > dune-workspace.afl
	@ dune build --workspace dune-workspace.afl --no-print-directory .

# [make setup] creates the required opam switch. Run it only once.
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
	opam install --yes afl-persistent pprint cppo ; \
	echo "Switching back to $$CURRENT..." ; \
	opam switch set $$CURRENT >/dev/null ; \

# [make clean] cleans up.

.PHONY: clean
clean:
	@ rm -rf $(OUTPUT)
	@ rm -f dune-workspace.afl
	@ make -C .. $@

# ----------------------------------------------------------------------------

# Settings.

# Directories for input and output files.
INPUT   := input
OUTPUT  := output
CRASHES := \
  $(wildcard $(OUTPUT)/crashes/id*) \
  $(wildcard $(OUTPUT)/*/crashes/id*) \

# This is where dune places the executable file.
BINARY  := ../_build/$(SWITCH)/fuzz/src/Main.exe

# On MacOS, the highest stack size that seems permitted is 65532.
STACK   := ulimit -s 65532

# ----------------------------------------------------------------------------

# [make test] runs afl-fuzz on a single CPU core.

.PHONY: test
test: all
	@ rm -rf $(OUTPUT)
	@ if [[ "$$OSTYPE" == "linux-gnu" ]]; then sudo ./setup-linux.sh ; fi
	@ $(STACK) && afl-fuzz -i $(INPUT) -o $(OUTPUT) $(BINARY) @@

# ----------------------------------------------------------------------------

# Find out how many hardware threads we have.

ifeq ($(shell echo $$OSTYPE),linux-gnu)
  # Gnu Linux.
  CORES  = $(shell grep -c ^processor /proc/cpuinfo)
else
  # Assuming MacOS.
  CORES  = $(shell sysctl -n hw.physicalcpu)
endif

# Use half of them minus one.
# On a machine with two hardware threads per core,
# this means we use all processor cores except one.
NSLAVES  = $(shell expr $(CORES) / 2 - 1)

# ----------------------------------------------------------------------------

# [make multicore] launches as many instances of afl-fuzz as there are cores.

.PHONY: multicore
multicore: all
	@ if [[ "$(NSLAVES)" == "0" ]]; then \
	    echo "Not enough cores! Run 'make test' instead."; exit 1; fi
# Run one instance in master mode, and many instances in slave mode.
	@ rm -rf $(OUTPUT) && mkdir $(OUTPUT)
	@ if [[ "$$OSTYPE" == "linux-gnu" ]]; then \
	    echo "Please authorize changing the processor settings for performance:" ; \
	    sudo ./setup-linux.sh ; \
	  fi
	@ echo "Launching one master instance..."
	@ $(STACK) && \
	    afl-fuzz -i $(INPUT) -o $(OUTPUT) -M master $(BINARY) @@ >/dev/null &
	@ echo "Launching $(NSLAVES) slave instances..."
	@ $(STACK) && \
	    for i in `seq $(NSLAVES)` ; do \
	      (afl-fuzz -i $(INPUT) -o $(OUTPUT) -S slave$$i $(BINARY) @@ >/dev/null &) ; \
	    done
# Display periodic progress reports.
	@ while true ; do \
	  sleep 5 ; \
	  afl-whatsup $(OUTPUT) ; \
	done

# [make kill] kills all afl-fuzz processes.

.PHONY: kill
kill:
	pkill afl-fuzz

# ----------------------------------------------------------------------------

# [make show] displays the problems found by afl-fuzz in the previous run.

.PHONY: show
show:
	@ $(STACK) && \
	  (for f in $(CRASHES) ; do \
	    echo $$f ; \
	    tmp=`mktemp /tmp/crash.XXXX` && \
	    ($(BINARY) $$f > $$tmp 2>&1 || true) >/dev/null 2>&1 ; \
	    cat $$tmp ; \
	    rm $$tmp ; \
	    echo ; \
	  done) | more

# [make summary] is like [make show], but postprocesses its output so as to
# keep only the last instruction before the crash, and sorts these lines, so
# as to determine the length of the shortest instruction sequence that causes
# a problem.

# If you determine that a crash can be obtained in (say) 4 instructions, then
# typing [make show] and searching for "@04: Failure" will allow you to
# inspect the scenario that caused this crash.

.PHONY: summary
summary:
	@ $(STACK) && \
	  (for f in $(CRASHES) ; do \
	    tmp=`mktemp /tmp/crash.XXXX` && \
	    ($(BINARY) $$f > $$tmp 2>&1 || true) >/dev/null 2>&1 ; \
	    cat $$tmp | grep "Failure" | head -n 1 ; \
	    rm $$tmp ; \
	  done) | sort | more

# ----------------------------------------------------------------------------

# [make min] attempts to minimize the problematic inputs found by
# afl-fuzz in the previous run.

# TEMPORARY do this in parallel!

.PHONY: min
min:
	COPY=`mktemp -d $(OUTPUT).XXXX` && rm -rf $(COPY) && \
	echo "Saving un-minimized output to $$COPY." && \
	cp -rf $(OUTPUT) $$COPY && \
	$(STACK) && \
	for f in $(CRASHES) ; do \
	  mv $$f $$f.original ; \
	  afl-tmin -i $$f.original -o $$f -- $(BINARY) @@ ; \
	  rm $$f.original ; \
	done

# ----------------------------------------------------------------------------

# [make keep] keeps the problematic inputs found by afl-fuzz in the previous
# run, so they are used as a starting point in future runs. (This enables a
# form of regression testing.)

.PHONY: keep
keep:
	cp $(CRASHES) $(INPUT)

# ----------------------------------------------------------------------------

# [make unload] turns off the MacOS Crash Reporter utility.
# [make load] turns it on again.
# This utility should be OFF for afl-fuzz to work correctly.

SL := /System/Library
PL := com.apple.ReportCrash

.PHONY: unload
unload:
	launchctl unload -w $(SL)/LaunchAgents/$(PL).plist
	sudo launchctl unload -w $(SL)/LaunchDaemons/$(PL).Root.plist

.PHONY: load
load:
	launchctl load -w $(SL)/LaunchAgents/$(PL).plist
	sudo launchctl load -w $(SL)/LaunchDaemons/$(PL).Root.plist
