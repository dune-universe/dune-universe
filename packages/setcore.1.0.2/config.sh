#!/bin/bash

aclocal -I m4 # probably doesn't work on OS X, ask Steve Jobs...
autoconf
autoheader
./configure
