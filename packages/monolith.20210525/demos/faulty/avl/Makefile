# Type [make random] to perform random testing
#  and [make test] to perform fuzzing (driven by afl-fuzz).

# When driven by afl-fuzz, it can take a minute to find a counterexample. When
# working in random mode, a counterexample is usually found in a few seconds.
# [make random] performs iterated narrowing (that is, it runs forever and
# searches for ever-shorter scenarios).

include ../../../Makefile.monolith
EXPECTING_BUGS := 1
TIMEOUT   := 60
SEED_SIZE := 1000
MODE := random
