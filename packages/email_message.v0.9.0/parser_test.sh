#!/bin/bash

# The output should be empty if everything is working all right

if [ $# -ne 1 ]
then
  echo "Usage: $0 <MBOX file> "
  exit 1
fi

INPUT=$1
OUTPUT_parsed="$1.parsed"
OUTPUT_parsed_parsed="$1.parsed.parsed"
bin/mailbox_test_pipe.exe -i $INPUT > $OUTPUT_parsed
bin/mailbox_test_pipe.exe -i $OUTPUT_parsed > $OUTPUT_parsed_parsed

diff $OUTPUT_parsed $OUTPUT_parsed_parsed

# Now, let's test the internal representation
OUTPUT_sexp="$1.sexp"
OUTPUT_parsed_sexp="$1.parsed.sexp"
bin/mailbox_test_pipe.exe -sexp -i $INPUT > $OUTPUT_sexp
bin/mailbox_test_pipe.exe -sexp -i $OUTPUT_parsed > $OUTPUT_parsed_sexp

diff $OUTPUT_sexp $OUTPUT_parsed_sexp


exit 0
