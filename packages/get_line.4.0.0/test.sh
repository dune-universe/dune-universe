#!/bin/bash

tmp=`mktemp`

seq 1 10 > $tmp

# set -x

# 1st line
diff <(./get_line -r 1 -i $tmp) <(seq 1 1)

# 2nd line
diff <(./get_line -r 2 -i $tmp) <(seq 2 2)

# 3rd line
diff <(./get_line -r 3 -i $tmp) <(seq 3 3)

# lines 2 to 5
diff <(./get_line -r 2..5 -i $tmp) <(seq 2 5)

# all but line 1
diff <(./get_line -r 1 -i $tmp -v) <(seq 2 10)

# all but line 2
diff <(./get_line -r 2 -i $tmp -v) <(seq 1 1; seq 3 10)

# all but line 3
diff <(./get_line -r 3 -i $tmp -v) <(seq 1 2; seq 4 10)

# all but lines 2 to 5
diff <(./get_line -r 2..5 -i $tmp -v) <(seq 1 1; seq 6 10)

# first three lines
diff <(./get_line -r +3 -i $tmp) <(seq 1 3)

# last three lines
diff <(./get_line -r -3 -i $tmp) <(seq 8 10)

# lines 1,5 and 10
diff <(./get_line -r 1,5,10 -i $tmp) <(printf "1\n5\n10\n")

set -x

#errors
./get_line -r 0 -i $tmp
./get_line -r 11 -i $tmp
./get_line -r 10..12 -i $tmp
./get_line -r 12..15 -i $tmp

#different each time
./get_line -r +10 -i $tmp --rand

set +x

rm -f $tmp
