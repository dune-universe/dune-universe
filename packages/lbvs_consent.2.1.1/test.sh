#!/bin/bash

set -x

# forget previous runs
\rm -f {opti,pess,real}.curr

# create an optimist consensus query
./consent -v -n 2 -s opti -q two_actives.csv -db two_actives.csv \
  2>/dev/null | tail -1 > opti.curr
# check it is equal to what it should be
diff opti.curr opti.ref

# same for pessimist strategy
./consent -v -n 2 -s pess -q two_actives.csv -db two_actives.csv \
  2>/dev/null | tail -1 > pess.curr
diff pess.curr pess.ref

# same for realist strategy
./consent -v -n 3 -s real -q three_actives.csv -db three_actives.csv \
  2>/dev/null | tail -1 > real.curr
diff real.curr real.ref
