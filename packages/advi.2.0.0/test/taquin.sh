#!/bin/sh

if [ -f ./taquin ]; then
  ./taquin $*
else
  ./sorry.wish $*
fi
