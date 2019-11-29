#!/bin/bash

set -x
set -e

# download and compile ranger

# HEAD
#git clone --depth 1 https://github.com/imbs-hl/ranger.git

# released-0.9.11
if [ ! -f 0.9.11.tar.gz ]; then
    wget https://github.com/imbs-hl/ranger/archive/0.9.11.tar.gz
fi
tar xzf 0.9.11.tar.gz

cd ranger-0.9.11/cpp_version
mkdir -p build
cd build
cmake ../
make -j `getconf _NPROCESSORS_ONLN`
cp ranger `opam config var bin`/ml_rf_ranger
