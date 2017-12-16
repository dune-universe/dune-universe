#!/bin/sh

# This is run inside the container to check the library builds

cd /src
opam pin add mirage-net-xen . -y
