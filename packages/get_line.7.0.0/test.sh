#!/bin/bash

#set -x # DEBUG

tmp=`mktemp`

seq 1 10 > $tmp

# set -x

GET_LINE=_build/default/src/get_line.exe

# 1st line
diff <($GET_LINE -r 1 -i $tmp) <(seq 1 1)

# 2nd line
diff <($GET_LINE -r 2 -i $tmp) <(seq 2 2)

# 3rd line
diff <($GET_LINE -r 3 -i $tmp) <(seq 3 3)

# lines 2 to 5
diff <($GET_LINE -r 2..5 -i $tmp) <(seq 2 5)

# all but line 1
diff <($GET_LINE -r 1 -i $tmp -v) <(seq 2 10)

# all but line 2
diff <($GET_LINE -r 2 -i $tmp -v) <(seq 1 1; seq 3 10)

# all but line 3
diff <($GET_LINE -r 3 -i $tmp -v) <(seq 1 2; seq 4 10)

# all but lines 2 to 5
diff <($GET_LINE -r 2..5 -i $tmp -v) <(seq 1 1; seq 6 10)

# first three lines
diff <($GET_LINE -r +3 -i $tmp) <(seq 1 3)

# last three lines
diff <($GET_LINE -r -3 -i $tmp) <(seq 8 10)

# lines 1,5 and 10
diff <($GET_LINE -r 1,5,10 -i $tmp) <(printf "1\n5\n10\n")

# lines 1,2 and 8,9,10
diff <($GET_LINE -r 2:3 -i $tmp) <(printf "1\n2\n8\n9\n10\n")

set -x

#errors
$GET_LINE -r 0 -i $tmp
$GET_LINE -r 11 -i $tmp
$GET_LINE -r 10..12 -i $tmp
$GET_LINE -r 12..15 -i $tmp

#different each time
$GET_LINE -r +10 -i $tmp --rand

set +x

rm -f $tmp
