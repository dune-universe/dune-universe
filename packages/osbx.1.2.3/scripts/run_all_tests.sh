#!/bin/bash

./time.sh ./encode.native

echo ""

echo "Single block encoding 21150 times"
./time.sh ./single_block_encode.native

echo ""

./time.sh ./decode.native

echo ""

echo "Single block decoding 21150 times"
./time.sh ./single_block_decode.native
