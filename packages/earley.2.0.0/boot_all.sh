#!/bin/bash

# Saving the initial switch
INITIAL_SWITCH=$(opam config var switch)

# Switching over all loops
for SWITCH in $(ls static/boot); do
  # Opam switching
  opam switch create ${SWITCH} 2> /dev/null || opam switch ${SWITCH}
  eval $(opam config env)
  echo "Bootstrapping for OCaml $(opam config var switch)"

  # Installing dependencies
  opam update
  opam install -y dune odoc

  # Bootstrap
  make distclean
  make boot

  # Cleaning up
  make distclean
  echo ""
done

# Restore initial switch
echo "Back to OCaml ${INITIAL_SWITCH}"
opam switch ${INITIAL_SWITCH}
eval $(opam config env)
