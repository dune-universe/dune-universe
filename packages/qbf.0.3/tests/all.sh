#!/usr/bin/env bash

ok=0; nok=0;
for p in test_*; do
    if ./$p; then
        printf "%20s OK\n" $p; ok=$(expr $ok + 1)
    else
        printf "%30s NOK\n" $p; nok=$(expr $nok + 1)
    fi
done
echo "Passed: $ok"
echo "Failed: $nok"
test $nok -eq 0
