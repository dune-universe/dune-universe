#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# A script that installs afl-fuzz.

cd /tmp
wget http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz
tar xvfz afl-latest.tgz
rm afl-latest.tgz
cd afl-*
make
# Ignore the dependency "install: all"
sudo make -o all install
