#!/usr/bin/env bash
set -ex
version="$1"
if [ -z "$version" ]; then
    echo Missing version argument. >/dev/fd/2
    exit 1
fi
major="${version%%.*}"
if [ "$major" -ge 10 ]; then
    padded_version="$version"
else
    padded_version="0$version"
fi
version_greater_than() {
    reference=$1
    minimum=`echo -e $padded_version'\n'$reference | sort | head -n 1`
    [ $minimum = $reference ]
}
if version_greater_than 09.0.1; then
    cfe=clang
    dir_suffix=.src
elif version_greater_than 03.4.1; then
    cfe=cfe
    dir_suffix=.src
else
    cfe=clang
    dir_suffix=
fi
if version_greater_than 03.5; then
    suffix=.tar.xz
else
    suffix=.tar.gz
fi
if version_greater_than 03.6; then
    CC=gcc
    CXX=g++
else
    if which gcc-4.9 >/dev/null 2>/dev/null; then
        CC=gcc-4.9
        CXX+=g++-4.9
    else
        CC=gcc-4.8
        CXX=g++-4.8
    fi
fi
if version_greater_than 09.0.1 || [ "$version" "=" 8.0.1 ]; then
    case "$version" in
    *rc*)
        version_dash="${version%rc*}-rc${version#*rc}"
        ;;
    *)
        version_dash="$version"
    esac
    base_url=\
"https://github.com/llvm/llvm-project/releases/download/llvmorg-$version_dash"
else
    case "$version" in
    *rc*)
        base_version="${version%rc*}"
        rc="${version#*rc}"
        base_url="https://prereleases.llvm.org/$base_version/rc$rc"
        ;;
    *)
        base_url="http://releases.llvm.org/$version"
    esac
fi
if [ ! -d llvm-$version$dir_suffix ]; then
    if [ ! -f llvm-$version.src$suffix ]; then
        wget "$base_url/llvm-$version.src$suffix"
    fi
    tar -xf llvm-$version.src$suffix
fi
if [ ! -d llvm-$version$dir_suffix/tools/clang ]; then
    if [ ! -d $cfe-$version$dir_suffix ]; then
        if [ ! -f $cfe-$version.src$suffix ]; then
            wget "$base_url/$cfe-$version.src$suffix"
        fi
        tar -xf $cfe-$version.src$suffix
    fi
    mv $cfe-$version$dir_suffix llvm-$version$dir_suffix/tools/clang
fi
mkdir llvm-$version.build || true
pushd llvm-$version.build
    cmake ../llvm-$version$dir_suffix \
        -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX -DLLVM_ENABLE_PIC=ON
    cmake --build .
popd
