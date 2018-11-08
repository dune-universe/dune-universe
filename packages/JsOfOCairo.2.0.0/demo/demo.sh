#!/bin/bash
# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

dune build --root=. draw_on_command_line.bc draw_in_browser.bc.js

_build/default/draw_on_command_line.bc

echo
echo "Have a look at $(pwd)/draw_in_browser.html"
