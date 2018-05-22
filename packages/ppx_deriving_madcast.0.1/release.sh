#!/bin/sh
set -euC
cd "$(dirname "$0")"

readonly LOCAL_OPAM_FILE=ppx_deriving_madcast.opam

opam_query () {
    opam query "$LOCAL_OPAM_FILE" "$@"
}

readonly VERSION=$(opam_query --version)
readonly NAME_VERSION=$(opam_query --name-version)
readonly ARCHIVE=$(opam_query --archive)

if [ -e "$NAME_VERSION" ]; then
    printf 'A file named `%s` exists already.\n' "$NAME_VERSION"
    exit 1
fi

printf 'Creating the tag v%s and pushing it to origin.\n' "$VERSION"
git tag -a "v$VERSION" -m "Version $VERSION."
git push origin "v$VERSION"

printf 'Creating `%s` and copying the basic files.\n' "$NAME_VERSION"
mkdir "$NAME_VERSION"
cp "$LOCAL_OPAM_FILE" "$NAME_VERSION"/opam
cp descr "$NAME_VERSION"/descr

printf 'Downloading the archive to compute the checksum.\n'
url="https://github.com/Niols/ppx_deriving_madcast/archive/${VERSION}.tar.gz"
sum=$(wget -qO- "$url" | md5sum | cut -d ' ' -f 1)
printf 'http: "%s"\nchecksum: "%s"\n' "$url" "$sum" \
       > "$NAME_VERSION"/url

printf 'Publishing the version to opam-repository.\n'
opam publish submit "$NAME_VERSION"
