#!/bin/bash

set -e -o pipefail

export OPAMROOT=$PWD/.opam-root

if [ ! -d $OPAMROOT ]; then
    echo "Creating opam root"
    opam init --root $OPAMROOT
fi

eval `opam config env --root $OPAMROOT`

opam update

PKG_LIST=$(mktemp)
rm -f $PKG_LIST
trap "rm -f $PKG_LIST" EXIT

for pkg in $(opam list --depends-on jbuilder -s); do
    echo ${pkg}...
    ver=$(opam info -f version  $pkg)
    dev=$(opam info -f dev-repo $pkg)
    echo "$pkg.$ver ${#pkg} $dev" >> $PKG_LIST
done

pkgs=$(cat $PKG_LIST | sort -n -k2 | sort -u -k3 | awk '{print $1}')

cd packages

# Remove old packages
for pkg in *; do
    if ! grep -q "^$pkg " $PKG_LIST; then
        echo "Deleting $pkg..."
        rm -rf $pkg
    fi
done

for pkg in $pkgs; do
    if  [ ! -d $pkg ]; then
        opam source $pkg
    fi
done
