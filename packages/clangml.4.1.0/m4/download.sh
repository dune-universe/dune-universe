#!/usr/bin/env bash
set -e -x
if which curl >/dev/null; then
    function download {
        target="$1"
        url="$2"
        curl "$url" > "$target"
    }
elif which wget >/dev/null; then
    function download {
        target="$1"
        url="$2"
        wget -O "$target" "$url"
    }
else
    function download {
        set +x
        >&2 echo "Please install curl or wget to download $2"
        exit 1
    }
fi

function download_if_needed {
    if [ ! -e "$1" ]; then
        download "$1" "$2"
    fi
}

target_dir=$(dirname "$0")

download_if_needed "$target_dir/ax_compare_version.m4" \
  'http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_compare_version.m4'
