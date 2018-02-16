#!/bin/bash

#set -x # DEBUG

rm -rf _build
jbuilder build @install
jbuilder install
