#!/bin/bash

. /home/tests/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
echo "########################################################################"
echo "########################### LIBS VERSIONS ##############################"
echo
echo $(opam list --short --columns=package installed ocaml)
echo $(opam list --short --columns=package installed ctypes)
echo $(opam list --short --columns=package installed ctypes-foreign)
echo $(opam list --short --columns=package installed ounit)
echo $(opam list --short --columns=package installed base)
echo $(opam list --short --columns=package installed stdio)
echo $(opam list --short --columns=package installed configurator)
echo "GObject-Introspection version: $(pkg-config --modversion gobject-introspection-1.0)"
echo
echo "########################################################################"
echo "########################################################################"

