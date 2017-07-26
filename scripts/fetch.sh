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
PKG_LIST_FILTERED=$(mktemp)
trap "rm -f $PKG_LIST $PKG_LIST_FILTERED" EXIT

for pkg in $(opam list --depends-on jbuilder -s); do
    # atdgen is replaced by atd
    if [[ $pkg == atdgen ]]; then
        echo "Skipping $pkg..."
    else
        echo ${pkg}...
        ver=$(opam info -f version  $pkg)
        dev=$(opam info -f dev-repo $pkg)
        echo "$pkg.$ver ${#pkg} $dev" >> $PKG_LIST
    fi
done

cat $PKG_LIST | sort -n -k2 | sort -u -k3 > $PKG_LIST_FILTERED
pkgs=$(awk '{print $1}' $PKG_LIST_FILTERED)

cd packages

# Remove old packages
for pkg in *; do
    if ! grep -q "^$pkg " $PKG_LIST_FILTERED; then
        echo "Deleting $pkg..."
        rm -rf $pkg
    fi
done

for pkg in $pkgs; do
    if  [ ! -d $pkg ]; then
        opam source $pkg
    fi
done
