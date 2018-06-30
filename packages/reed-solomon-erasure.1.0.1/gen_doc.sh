#!/bin/bash

rm -rf doc

mkdir doc

jbuilder build @doc

cp -r _build/default/_doc/_html/* doc/
