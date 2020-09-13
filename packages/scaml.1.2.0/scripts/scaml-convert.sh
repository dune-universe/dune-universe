#!/bin/sh

# Execute a contract to evaluate SCaml expression $1 and store the result to its storage.

# Example
#
# $ ./scaml-convert.sh 'Int 1 + Int 2'
# Making /.../x.ml ...
# open SCaml
# let main () _ = [], Some (Int 1 + Int 2)
# Compiling... done
# Executing /.../x.tz ...
# storage
#   (Some 3)  <==== Result in Tezos value with Some
# emitted operations
#   
# big_map diff
#   

set -e

# Disable the disclaimer message of tezos-node
export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y 

tmpdir=$(mktemp -d)
cat > $tmpdir/x.ml <<EOF
open SCaml
let main () _ = [], Some ($1)
EOF
/bin/echo Making $tmpdir/x.ml ...
cat $tmpdir/x.ml
echo Compiling...
scamlc $tmpdir/x.ml
echo " done"
echo Executing $tmpdir/x.tz ...
tezos-client run script $tmpdir/x.tz on storage None and input Unit
