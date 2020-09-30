#!/bin/bash

# Check for long lines.
#awk 'length>78    {print "In " FILENAME ", line " FNR " more than 78 characters..."}' \
#  lib/*.ml lib/*.mli pa_ocaml/*.ml

# Check for trailing spaces.
awk '/.*\s$/      {print "In " FILENAME ", line " FNR " has trailing spaces..."}    ' \
  lib/*.ml lib/*.mli pa_ocaml/*.ml

# Check for tabulations.
awk '/.*\t.*/     {print "In " FILENAME ", line " FNR " contains tabulations..."}   ' \
  lib/*.ml lib/*.mli pa_ocaml/*.ml
