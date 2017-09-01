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
trap "rm -f $PKG_LIST $PKG_LIST" EXIT

# Don't use "--depends-on jbuilder" as this doesn't list packages that
# are not available on the current OS
opam info -f package,version,dev-repo,depends $(opam list -As) | \
    grep -B3 '^ *depends:.*\bjbuilder\b' | grep -v '^--$' | {
    while read key pkg; do
        [[ $key == "package:" ]]
        read key ver
        [[ $key == "version:" ]]
        read key dev
        [[ $key == "dev-repo:" ]]
        read key deps
        [[ $key == "depends:" ]]
        case $pkg in
	    # atdgen is replaced by atd
	    atdgen)
		;;
	    *)
                echo "$pkg.$ver ${#pkg} $dev"
                ;;
        esac
    done
} | sort -n -k2 | sort -u -k3 > $PKG_LIST

cd packages

# Remove old packages
for pkg in *; do
    if ! grep -q "^$pkg " $PKG_LIST; then
        echo "Deleting $pkg..."
        rm -rf $pkg
    fi
done

# Fetch new ones
for pkg in $(awk '{print $1}' $PKG_LIST); do
    if  [ ! -d $pkg ]; then
        opam source $pkg
    fi
done
